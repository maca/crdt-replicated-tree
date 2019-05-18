module RG exposing
  ( RG
  , Error
  , init
  , add
  , addBranch
  , delete
  , batch
  , apply
  , operationsSince
  , lastOperation
  , get
  )

{-| `RG` is a Replicated Graph, it keeps the local replica
state.
The timestamp for adding nodes is calculated by adding
`maxReplicas` count to the last timestamp, and the initial
timestamp corresponds to the `ReplicaId`.
This sets two constraints: the `ReplicaId` has to be unique for
each replica, and the maximum number of replicas has to be
declared.

# Init

@docs RG
@docs init

# Operations

@docs Error
@docs add
@docs addBranch
@docs delete
@docs batch
@docs apply
@docs operationsSince
@docs lastOperation

# Nodes

@docs get

-}

import Dict exposing (Dict, keys)
import List exposing (head)
import Result

import RG.Node as Node exposing (Node(..))
import RG.List exposing
  ( Error(..)
  , replaceWhen
  , insertWhen
  , applyWhen
  , find
  )
import RG.Operation as Operation exposing (Operation(..))
import RG.ReplicaId as ReplicaId exposing (ReplicaId)


{-| Opaque type representing a Replicated Graph,
to initialize see [int](#init).
-}
type RG a =
  RG
    { replicaId: ReplicaId
    , maxReplicas: Int
    , root : Node a
    , timestamp: Int
    , pointer: List Int
    , operations: List (Operation a)
    , replicas: Dict Int Int
    , lastOperation: Operation a
    }


{-| Failure to apply an operation.
-}
type Error =
  Error
    { replicaId: ReplicaId
    , timestamp: Int
    , path: List Int
    }


type alias UpdateFun a =
  Maybe Int -> List (Node a) -> Result RG.List.Error (List (Node a))


type alias NodeFun a =
  List Int -> Maybe Int -> Node a


{-| Build a RG
-}
init : { id: Int, maxReplicas: Int } -> RG a
init {id, maxReplicas} =
  RG
    { replicaId = ReplicaId.fromInt id
    , maxReplicas = maxReplicas
    , operations = []
    , pointer = [0]
    , replicas = Dict.empty
    , root = Node.root
    , timestamp = id
    , lastOperation = Batch []
    }


{-| Build and add a node after pointer position
-}
add : a -> RG a -> Result Error (RG a)
add value (RG record as graph) =
  let
      {timestamp, replicaId} = record

      newTimestamp = nextTimestamp graph timestamp
  in
      applyLocal (Add replicaId newTimestamp record.pointer value) graph


{-| Build and add a branch after pointer position
-}
addBranch : a -> RG a -> Result Error (RG a)
addBranch value rga =
  add value rga |> Result.map branchPointer


{-| Delete a node
-}
delete : List Int -> RG a -> Result Error (RG a)
delete path (RG record as graph) =
  applyLocal (Delete record.replicaId path) graph


{-| Apply a list of operations
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
            Err <|
              Error
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
        Add replica timestamp path value ->
          let
              nodePath =
                List.reverse path
                  |> List.tail
                  |> Maybe.withDefault []
                  |> ((::) timestamp)
                  |> List.reverse

              fun =
                addFun value nodePath
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


addFun : a -> List Int
           -> Maybe Int
           -> List (Node a)
           -> Result RG.List.Error (List (Node a))
addFun value path maybePreviousTs nodes =
  let
      node =
        Node.init value path

      timestamp =
        Node.timestamp node

  in
      case maybePreviousTs of
        Just previousTs ->
          insertWhen (Node.hasTimestamp previousTs) node nodes

        Nothing ->
          case find (\n -> (Node.timestamp n) == timestamp) nodes of
            Just _ ->
              Err AlreadyApplied

            Nothing ->
              Ok [ node ]


deleteFun : List Int -> Maybe Int
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


updateBranch : UpdateFun a -> List Int
                           -> Node a
                           -> Result RG.List.Error (Node a)
updateBranch fun path parent =
  case parent of
    Tombstone _ ->
      Err AddToTombstone

    Root {children} ->
      updateBranchHelp fun path parent children

    Node {children} ->
      updateBranchHelp fun path parent children


updateBranchHelp fun path parent children =
  case path of
    [] ->
      Err NotFound

    [0] ->
      fun Nothing children |> updateChildren parent

    ts :: [] ->
      fun (Just ts) children |> updateChildren parent

    ts :: tss ->
      let
          update node =
            updateBranch fun tss node
              |> Result.map List.singleton
      in
          applyWhen (Node.hasTimestamp ts) update children
            |> updateChildren parent


updateChildren : Node a -> Result RG.List.Error (List (Node a))
                        -> Result RG.List.Error (Node a)
updateChildren parent result =
  Result.map (\children -> Node.updateChildren children parent) result


branchPointer : RG a -> RG a
branchPointer (RG record) =
  RG { record | pointer = record.pointer ++ [0] }


mergeLastOperation : RG a -> RG a -> RG a
mergeLastOperation (RG record1) (RG record2) =
  let
      operations1 = record1.lastOperation
      operations2 = record2.lastOperation
      operation   = Operation.merge operations1 operations2
  in
    RG { record2 | lastOperation = operation }


updatePointer : Int -> List Int -> RG a -> RG a
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


{-| Return the last successfully applied operation
-}
lastOperation : RG a -> Operation a
lastOperation (RG record) =
  record.lastOperation


{-| Return a list of operations since a timestamp
-}
operationsSince : Int -> RG a -> List (Operation a)
operationsSince timestamp (RG record) =
  case timestamp of
    0 ->
      record.operations |> List.reverse

    _ ->
      Operation.since timestamp record.operations


{-| Get a value at path
-}
get : List Int -> RG a -> Maybe a
get path (RG record) =
  Node.descendant path record.root |> Maybe.andThen Node.value


buildPath : Int -> List Int -> List Int
buildPath timestamp path =
  case List.reverse path of
    [] ->
      [ timestamp ]

    _ :: rest ->
      List.reverse <| timestamp :: rest

