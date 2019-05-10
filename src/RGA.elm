module RGA exposing
  ( Operation(..)
  , ReplicaId(..)
  , RGA
  , Error
  , init
  , add
  , addBranch
  , delete
  , batch
  , apply
  , applyLocal
  , operationsSince
  , lastReplicaTimestamp
  , nextTimestamp
  )

{-| Implementation of RGA (a Conflict-free Replicated Datatype)
modified to deal with distributed graphs.

CRDTs are datatypes that can be replicated across many nodes, edited
independently and reconciled excluding the posiblity of conflict.
Making them useful for applications where state has to be syncronized
with no manual conflict resolution, such as distributed text editors.

Each element in a Replicated Growing Array is identified by an
incrementing counter or vector clock. 
Only two operations are allowed: adding after another element, and
deleting an existing element, marking it as a tombstone.

-}

import Dict exposing (Dict, keys)
import List exposing (head)
import Result

import RGA.Node as Node exposing (Node(..), NodeData, Path)
import RGA.List exposing
  ( ListFail(..)
  , replaceWhen
  , insertWhen
  , applyWhen
  , find
  )


{-| Represent a Replicated Distributed Graph
-}
type alias RGA a =
  { replicaId: ReplicaId
  , maxReplicas: Int
  , root : Node a
  , timestamp: Int
  , pointer: Path
  , operations: List (Operation a)
  , replicas: Dict Int Int
  , lastOperation: Operation a
  }


{-| The id for a replica, should be unique per replica
-}
type ReplicaId = ReplicaId Int


{-| Error for an attempted operation
-}
type alias Error =
  { replicaId: ReplicaId
  , timestamp: Int
  , path: Path
  }


{-| Represents an RGA operation
-}
type Operation a
  = Add ReplicaId Int Path (Maybe a)
  | Delete ReplicaId Path
  | Batch (List (Operation a))


type alias UpdateFun a =
  Maybe Int -> List (Node a) -> Result ListFail (List (Node a))


type alias NodeFun a =
  Path -> Maybe Int -> Node a


{-| Represents an RGA operation
-}
add : Maybe a -> RGA a -> Result Error (RGA a)
add maybeA ({timestamp, replicaId, pointer} as rga) =
  let
      newTimestamp = nextTimestamp rga timestamp
  in
      applyLocal (Add rga.replicaId newTimestamp pointer maybeA) rga


addBranch : Maybe a -> RGA a -> Result Error (RGA a)
addBranch maybeA rga =
  add maybeA rga |> Result.map branchPointer


delete : Path -> RGA a -> Result Error (RGA a)
delete path ({replicaId} as rga) =
  applyLocal (Delete replicaId path) rga


batch : List (RGA a -> Result Error (RGA a)) -> RGA a
                                             -> Result Error (RGA a)
batch funs rga =
  applyBatch funs rga


apply : Operation a -> RGA a -> Result Error (RGA a)
apply operation rga =
  applyLocal operation rga
    |> Result.map (\r -> { r | pointer = rga.pointer })


applyLocal : Operation a -> RGA a -> Result Error (RGA a)
applyLocal operation rga =
  let
      mapResult replicaId timestamp path result =
        case result of
          Err AlreadyApplied ->
            Ok { rga | lastOperation = Batch [] }

          Err _ ->
            Err
              { replicaId = replicaId
              , timestamp = timestamp
              , path = path
              }

          Ok root ->
            let
                update =
                  updateTimestamp replicaId timestamp
                    >> appendOperation operation
                    >> updatePointer timestamp path
            in
                Ok <| update { rga | root = root }
  in
      case operation of
        Add replica timestamp path data ->
          let
              nodePath =
                List.reverse path
                  |> List.tail
                  |> Maybe.withDefault []
                  |> ((::) timestamp)
                  |> List.reverse

              fun =
                addFun data timestamp nodePath
          in
              updateBranch fun path rga.root
                |> mapResult replica timestamp path

        Delete replica path ->
          let
              timestamp =
                List.reverse path
                  |> List.head
                  |> Maybe.withDefault 0

              fun =
                deleteFun path
          in
              updateBranch fun path rga.root
                |> mapResult replica timestamp path

        Batch ops ->
          applyBatch (List.map apply ops) rga


applyBatch funcs rga =
  batchFold rga (Ok { rga | lastOperation = Batch [] }) funcs


batchFold : RGA a -> Result Error (RGA a)
                  -> List (RGA a -> Result Error (RGA a))
                  -> Result Error (RGA a)
batchFold rga result opFuns =
  case opFuns of
    [] ->
      result

    f :: fs ->
      let
          fun = f >> Result.map2 mergeLastOperation result
      in
          batchFold rga ((Result.andThen fun) result) fs


addFun : Maybe a -> Int
                 -> Path
                 -> Maybe Int
                 -> List (Node a)
                 -> Result ListFail (List (Node a))
addFun maybeA timestamp path maybePreviousTs nodes =
  let
      node = Node.node maybeA timestamp path
  in
      case maybePreviousTs of
        Just previousTs ->
          insertWhen (Node.match previousTs) node nodes

        Nothing ->
          Ok [ node ]


deleteFun : Path -> Maybe Int
                 -> List (Node a)
                 -> Result ListFail (List (Node a))
deleteFun path maybePreviousTs nodes =
  case maybePreviousTs of
    Just previousTs ->
      let
          node = Node.tombstone previousTs path
          pred = Node.match previousTs
      in
          replaceWhen pred node nodes

    Nothing ->
      Err NotFound


updateBranch : UpdateFun a -> Path
                           -> Node a
                           -> Result ListFail (Node a)
updateBranch fun path parent =
  case parent of
    Tombstone _ ->
      Err NotFound

    Node ({children} as nodeData) ->
      let
          updateChildren =
            Result.map (\c -> Node { nodeData | children = c })
      in
          case path of
            [] ->
              Err NotFound

            [0] ->
              fun Nothing children |> updateChildren

            ts :: [] ->
              fun (Just ts) children |> updateChildren

            ts :: tss ->
              let
                  branchFun node =
                    updateBranch fun tss node
                      |> Result.map List.singleton
              in
                  applyWhen (Node.match ts) branchFun children
                    |> updateChildren


branchPointer : RGA a -> RGA a
branchPointer rga =
  { rga | pointer = rga.pointer ++ [0] }


mergeLastOperation : RGA a -> RGA a -> RGA a
mergeLastOperation r1 r2 =
  let
      ops1 = r1.lastOperation
      ops2 = r2.lastOperation
      operation = mergeOperations ops1 ops2
  in
    { r2 | lastOperation = operation }


updatePointer : Int -> Path -> RGA a -> RGA a
updatePointer timestamp path rga =
  { rga | pointer = buildPath timestamp path }


appendOperation : Operation a -> RGA a -> RGA a
appendOperation operation rga =
  { rga | operations = operation :: rga.operations
  , lastOperation = operation
  }


updateTimestamp : ReplicaId -> Int -> RGA a -> RGA a
updateTimestamp (ReplicaId id) opTs rga =
  let
      timestamp =
        mergeTimestamp rga rga.timestamp opTs
  in
      { rga | timestamp = timestamp
      , replicas = Dict.insert id opTs rga.replicas
      }


mergeTimestamp : RGA a -> Int -> Int -> Int
mergeTimestamp rga timestamp operationTimestamp =
  if timestamp >= operationTimestamp then
    timestamp
  else
    let
        next =
          nextTimestamp rga timestamp
    in
        mergeTimestamp rga next operationTimestamp


nextTimestamp : RGA a -> Int -> Int
nextTimestamp {maxReplicas} timestamp =
  timestamp + (if maxReplicas < 2 then 1 else maxReplicas - 1)


buildPath : Int -> Path -> Path
buildPath timestamp path =
  case List.reverse path of
    [] ->
      [ timestamp ]

    _ :: rest ->
      List.reverse <| timestamp :: rest


operationsSince : Int -> RGA a -> List (Operation a)
operationsSince ts {operations} =
  operationsSinceFold ts operations []


operationsSinceFold : Int -> List (Operation a)
                          -> List (Operation a)
                          -> List (Operation a)
operationsSinceFold timestamp operations acc =
  case operations of
    [] -> []

    o :: os ->
      case o of
        Batch _ ->
          operationsSinceFold timestamp os acc

        Delete _ _ ->
          operationsSinceFold timestamp os (o :: acc)

        Add _ operationTimestamp _ _ ->
          if timestamp == operationTimestamp then
            o :: acc

          else
            operationsSinceFold timestamp os (o :: acc)


lastReplicaTimestamp : Int -> RGA a -> Int
lastReplicaTimestamp rid {replicas} =
  Dict.get rid replicas |> Maybe.withDefault -1


{-| Build an RGA
-}
init : { id: Int, maxReplicas: Int } -> RGA a
init {id, maxReplicas} =
  let
      operation = Add (ReplicaId id) -1 [0] Nothing
      rga =
        { replicaId = ReplicaId id
        , maxReplicas = maxReplicas
        , operations = []
        , pointer = []
        , replicas = Dict.empty
        , root = Node.root
        , timestamp = id
        , lastOperation = Batch []
        }
  in
      applyLocal operation rga
        |> Result.map branchPointer
        |> Result.withDefault rga


operationToList : Operation a -> List (Operation a)
operationToList operation =
  case operation of
    Add _ _ _ _ -> [ operation ]
    Delete _ _ -> [ operation ]
    Batch list -> list


mergeOperations : Operation a -> Operation a -> Operation a
mergeOperations a b =
  Batch ((operationToList a) ++ (operationToList b))


