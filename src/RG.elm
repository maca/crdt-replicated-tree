module RG exposing
  ( RG(..)
  , Error
  , init
  , add
  , addBranch
  , delete
  , batch
  , apply
  )

{-| `RG` is a Replicated Graph, it keeps the local replica
state.
The timestamp for adding nodes is calculated by adding
`maxReplicas` count to the last timestamp, and the initial
timestamp corresponds to the `ReplicaId`.
This sets two constraints: the `ReplicaId` has to be unique for
each replica, and the maximum number of replicas has to be
declared.

@docs RG
@docs Error
@docs init
@docs add
@docs addBranch
@docs delete
@docs batch
@docs apply

-}

import Dict exposing (Dict, keys)
import List exposing (head)
import Result

import RG.Node as Node exposing (Node(..), Path)
import RG.List exposing
  ( Error(..)
  , replaceWhen
  , insertWhen
  , applyWhen
  , find
  )
import RG.Operation exposing (Operation(..))
import RG.ReplicaId as ReplicaId exposing (ReplicaId)


{-| Represent a Replicated Graph
-}
type RG a =
  RG
    { replicaId: ReplicaId
    , maxReplicas: Int
    , root : Node a
    , timestamp: Int
    , pointer: Path
    , operations: List (Operation a)
    , replicas: Dict Int Int
    , lastOperation: Operation a
    }


{-| Extra information about a failure when applying an
operation

`replicaId` is the id of the replica where the operation was
generated

`timestamp` is the timestamp of the attempted operation

`path` is the path of the existing node, either a node intended
to be `deleted` or the previous node for an `add` operation

If an operation fails it is usefull knowing the replica where
the operation was generated, to request the operations since the
last know operation.

-}
type alias Error =
  { replicaId: ReplicaId
  , timestamp: Int
  , path: Path
  }


type alias UpdateFun a =
  Maybe Int -> List (Node a) -> Result RG.List.Error (List (Node a))


type alias NodeFun a =
  Path -> Maybe Int -> Node a


{-| Build and add a node after pointer position
-}
add : Maybe a -> RG a -> Result Error (RG a)
add maybeA (RG record as graph) =
  let
      {timestamp, replicaId, pointer} = record

      newTimestamp = nextTimestamp graph timestamp
  in
      applyLocal (Add replicaId newTimestamp pointer maybeA) graph


{-| Build and add a branch after pointer position
-}
addBranch : Maybe a -> RG a -> Result Error (RG a)
addBranch maybeA rga =
  add maybeA rga |> Result.map branchPointer


{-| Mark a RG node as a `Tombstone`
-}
delete : Path -> RG a -> Result Error (RG a)
delete path (RG record as graph) =
  applyLocal (Delete record.replicaId path) graph


{-| Apply a list of functions that edit a RG to a RG
-}
batch : List (RG a -> Result Error (RG a)) -> RG a
                                           -> Result Error (RG a)
batch funs rga =
  applyBatch funs rga


{-| Apply a remote operation
-}
apply : Operation a -> RG a -> Result Error (RG a)
apply operation graph =
  applyLocal operation graph
    |> Result.map (\(RG record) ->
        RG { record | pointer = record.pointer })


{-| Apply a local operation, the pointer for the `RG` will
change
-}
applyLocal : Operation a -> RG a -> Result Error (RG a)
applyLocal operation (RG record as graph) =
  let
      mapResult replicaId timestamp path result =
        case result of
          Err AlreadyApplied ->
            Ok <| RG { record | lastOperation = Batch [] }

          Err AddToTombstone ->
            Ok <| RG { record | lastOperation = Batch [] }

          Err exp ->
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
                Ok <| update <| RG { record | root = root }
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
                addFun data nodePath
          in
              updateBranch fun path record.root
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
              updateBranch fun path record.root
                |> mapResult replica timestamp path

        Batch ops ->
          applyBatch (List.map apply ops) graph


applyBatch funcs (RG record as graph) =
  batchFold graph funcs (Ok <| RG { record | lastOperation = Batch [] })


batchFold : RG a -> List (RG a -> Result Error (RG a))
                 -> Result Error (RG a)
                 -> Result Error (RG a)
batchFold rga opFuns result =
  case opFuns of
    [] ->
      result

    f :: fs ->
      let
          fun = f >> Result.map2 mergeLastOperation result
      in
          batchFold rga fs ((Result.andThen fun) result)


addFun : Maybe a -> Path
                 -> Maybe Int
                 -> List (Node a)
                 -> Result RG.List.Error (List (Node a))
addFun maybeA path maybePreviousTs nodes =
  let
      node = Node.init maybeA path
  in
      case maybePreviousTs of
        Just previousTs ->
          insertWhen (Node.hasTimestamp previousTs) node nodes

        Nothing ->
          Ok [ node ]


deleteFun : Path -> Maybe Int
                 -> List (Node a)
                 -> Result RG.List.Error (List (Node a))
deleteFun path maybePreviousTs nodes =
  case maybePreviousTs of
    Just previousTs ->
      let
          node = Node.tombstone path
          pred = Node.hasTimestamp previousTs
      in
          replaceWhen pred node nodes

    Nothing ->
      Err NotFound


updateBranch : UpdateFun a -> Path
                           -> Node a
                           -> Result RG.List.Error (Node a)
updateBranch fun path parent =
  case parent of
    Tombstone _ ->
      Err AddToTombstone

    Node ({children} as payload) ->
      let
          updateChildren =
            Result.map (\c -> Node { payload | children = c })
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
                  applyWhen (Node.hasTimestamp ts) branchFun children
                    |> updateChildren


branchPointer : RG a -> RG a
branchPointer (RG record) =
  RG { record | pointer = record.pointer ++ [0] }


mergeLastOperation : RG a -> RG a -> RG a
mergeLastOperation (RG record1) (RG record2) =
  let
      operations1 = record1.lastOperation
      operations2 = record2.lastOperation
      operation   = mergeOperations operations1 operations2
  in
    RG { record2 | lastOperation = operation }


updatePointer : Int -> Path -> RG a -> RG a
updatePointer timestamp path (RG record) =
  RG { record | pointer = buildPath timestamp path }


appendOperation : Operation a -> RG a -> RG a
appendOperation operation (RG record) =
  RG
    { record | operations = operation :: record.operations
    , lastOperation = operation
    }


updateTimestamp : ReplicaId -> Int -> RG a -> RG a
updateTimestamp replicaId operationTimestamp (RG record as graph) =
  let
      timestamp =
        mergeTimestamp graph record.timestamp operationTimestamp

      id =
        ReplicaId.toInt replicaId
  in
      RG
        { record | timestamp = timestamp
        , replicas = Dict.insert id operationTimestamp record.replicas
        }


mergeTimestamp : RG a -> Int -> Int -> Int
mergeTimestamp rga timestamp operationTimestamp =
  if timestamp >= operationTimestamp then
    timestamp
  else
    let
        next =
          nextTimestamp rga timestamp
    in
        mergeTimestamp rga next operationTimestamp


{-| Get the next timestamp
-}
nextTimestamp : RG a -> Int -> Int
nextTimestamp (RG record) timestamp =
  timestamp + (if record.maxReplicas < 2 then 1 else record.maxReplicas - 1)


buildPath : Int -> Path -> Path
buildPath timestamp path =
  case List.reverse path of
    [] ->
      [ timestamp ]

    _ :: rest ->
      List.reverse <| timestamp :: rest


{-| Build a RG
-}
init : { id: Int, maxReplicas: Int } -> RG a
init {id, maxReplicas} =
  let
      replicaId =
        ReplicaId.fromInt id

      operation =
        Add replicaId -1 [0] Nothing

      graph =
        RG
          { replicaId = replicaId
          , maxReplicas = maxReplicas
          , operations = []
          , pointer = []
          , replicas = Dict.empty
          , root = Node.root
          , timestamp = id
          , lastOperation = Batch []
          }
  in
      applyLocal operation graph
        |> Result.map branchPointer
        |> Result.withDefault graph


operationToList : Operation a -> List (Operation a)
operationToList operation =
  case operation of
    Add _ _ _ _ -> [ operation ]
    Delete _ _ -> [ operation ]
    Batch list -> list


mergeOperations : Operation a -> Operation a -> Operation a
mergeOperations a b =
  Batch ((operationToList a) ++ (operationToList b))
