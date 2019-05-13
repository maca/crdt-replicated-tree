module RG exposing
  ( RG
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
import RG.Operation exposing (Operation(..), ReplicaId(..))


{-| Represent a Replicated Graph

`replicaId` is the local replica id

`maxReplicas` maximum number of replicas

`root` root node

`timestamp` timestamp of the last operation

`pointer` path of the last added node

`operations` cache of all applied operations

`replicas` known replicas and their last known timestamp

`lastOperation` last succesfully applied operation or batch

-}
type alias RG a =
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
add maybeA ({timestamp, replicaId, pointer} as rga) =
  let
      newTimestamp = nextTimestamp rga timestamp
  in
      applyLocal (Add rga.replicaId newTimestamp pointer maybeA) rga


{-| Build and add a branch after pointer position
-}
addBranch : Maybe a -> RG a -> Result Error (RG a)
addBranch maybeA rga =
  add maybeA rga |> Result.map branchPointer


{-| Mark an RG node as a `Tombstone`
-}
delete : Path -> RG a -> Result Error (RG a)
delete path ({replicaId} as rga) =
  applyLocal (Delete replicaId path) rga


{-| Apply a list of functions that edit an RG to an RG
-}
batch : List (RG a -> Result Error (RG a)) -> RG a
                                           -> Result Error (RG a)
batch funs rga =
  applyBatch funs rga


{-| Apply a remote operation
-}
apply : Operation a -> RG a -> Result Error (RG a)
apply operation rga =
  applyLocal operation rga
    |> Result.map (\r -> { r | pointer = rga.pointer })


{-| Apply a local operation, the pointer for the `RG` will
change
-}
applyLocal : Operation a -> RG a -> Result Error (RG a)
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


batchFold : RG a -> Result Error (RG a)
                 -> List (RG a -> Result Error (RG a))
                 -> Result Error (RG a)
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
                 -> Result RG.List.Error (List (Node a))
addFun maybeA timestamp path maybePreviousTs nodes =
  let
      node = Node.node maybeA timestamp path
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
          node = Node.tombstone previousTs path
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
      Err NotFound

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
branchPointer rga =
  { rga | pointer = rga.pointer ++ [0] }


mergeLastOperation : RG a -> RG a -> RG a
mergeLastOperation r1 r2 =
  let
      ops1 = r1.lastOperation
      ops2 = r2.lastOperation
      operation = mergeOperations ops1 ops2
  in
    { r2 | lastOperation = operation }


updatePointer : Int -> Path -> RG a -> RG a
updatePointer timestamp path rga =
  { rga | pointer = buildPath timestamp path }


appendOperation : Operation a -> RG a -> RG a
appendOperation operation rga =
  { rga | operations = operation :: rga.operations
  , lastOperation = operation
  }


updateTimestamp : ReplicaId -> Int -> RG a -> RG a
updateTimestamp (ReplicaId id) opTs rga =
  let
      timestamp =
        mergeTimestamp rga rga.timestamp opTs
  in
      { rga | timestamp = timestamp
      , replicas = Dict.insert id opTs rga.replicas
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
nextTimestamp {maxReplicas} timestamp =
  timestamp + (if maxReplicas < 2 then 1 else maxReplicas - 1)


buildPath : Int -> Path -> Path
buildPath timestamp path =
  case List.reverse path of
    [] ->
      [ timestamp ]

    _ :: rest ->
      List.reverse <| timestamp :: rest


{-| Build an RG
-}
init : { replicaId: Int, maxReplicas: Int } -> RG a
init {replicaId, maxReplicas} =
  let
      operation = Add (ReplicaId replicaId) -1 [0] Nothing
      rga =
        { replicaId = ReplicaId replicaId
        , maxReplicas = maxReplicas
        , operations = []
        , pointer = []
        , replicas = Dict.empty
        , root = Node.root
        , timestamp = replicaId
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


