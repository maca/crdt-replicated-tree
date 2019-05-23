module CRDTree exposing
  ( CRDTree
  , Error
  , init
  , add
  , addBranch
  , delete
  , batch
  , apply
  , operationsSince
  , lastOperation
  , id
  , root
  , get
  , cursor
  , moveCursorUp
  )

{-| `CRDTree` is a Replicated Tree, it keeps the local replica
state.
The timestamp for adding nodes is calculated by adding
`maxReplicas` count to the last timestamp, and the initial
timestamp corresponds to the `ReplicaId`.
This sets two constraints: the `ReplicaId` has to be unique for
each replica, and the maximum number of replicas has to be
declared.

# Init

@docs CRDTree
@docs init

# Operations

@docs add
@docs addBranch
@docs delete
@docs batch
@docs apply
@docs operationsSince
@docs lastOperation

# Tree

@docs id
@docs root
@docs get
@docs cursor
@docs moveCursorUp

-}

import Dict exposing (Dict, keys)
import List exposing (head)
import Result

import CRDTree.Node as Node exposing (Node(..))
import CRDTree.List exposing
  ( Error(..)
  , replaceWhen
  , insertWhen
  , applyWhen
  , find
  )
import CRDTree.Operation as Operation exposing (Operation(..))
import CRDTree.ReplicaId as ReplicaId exposing (ReplicaId)



{-| Opaque type representing a Replicated Tree,
to build see [int](#init).
-}
type CRDTree a =
  CRDTree
    { replicaId: ReplicaId
    , maxReplicas: Int
    , root : Node a
    , timestamp: Int
    , cursor: List Int
    , operations: List (Operation a)
    , replicas: Dict Int Int
    , lastOperation: Operation a
    }


{-| Failure to apply an operation.
-}
type alias Error =
  { replicaId: ReplicaId
  , timestamp: Int
  , path: List Int
  }


type alias UpdateFun a =
  Maybe Int -> List (Node a)
            -> Result CRDTree.List.Error (List (Node a))


type alias NodeFun a =
  List Int -> Maybe Int -> Node a


{-| Build a CRDTree
-}
init : { id: Int, maxReplicas: Int } -> CRDTree a
init params =
  CRDTree
    { replicaId = ReplicaId.fromInt params.id
    , maxReplicas = params.maxReplicas
    , operations = []
    , cursor = [0]
    , replicas = Dict.empty
    , root = Node.root
    , timestamp = params.id
    , lastOperation = Batch []
    }


{-| Build and add a node after tree cursor

    init { id = 1, maxReplicas = 4 }
      |> add 'a'

-}
add : a -> CRDTree a -> Result Error (CRDTree a)
add value (CRDTree record as tree) =
  let
      newTimestamp = nextTimestamp tree record.timestamp
  in
      applyLocal (Add record.replicaId newTimestamp record.cursor value) tree


{-| Build and add a branch after tree cursor
-}
addBranch : a -> CRDTree a -> Result Error (CRDTree a)
addBranch value rga =
  add value rga |> Result.map branchCursor


{-| Delete a node

    init { id = 1, maxReplicas = 4 }
      |> add 'a'
      |> Result.map (add 'b')
-}
delete : List Int -> CRDTree a -> Result Error (CRDTree a)
delete path (CRDTree record as tree) =
  applyLocal (Delete record.replicaId path) tree


{-| Apply a list of operations
-}
batch : List (CRDTree a -> Result Error (CRDTree a))
      -> CRDTree a
      -> Result Error (CRDTree a)
batch funs rga =
  applyBatch funs rga


{-| Apply a remote operation
-}
apply : Operation a -> CRDTree a -> Result Error (CRDTree a)
apply operation tree =
  applyLocal operation tree
    |> Result.map (\(CRDTree record) ->
        CRDTree { record | cursor = record.cursor })


{-| Apply a local operation, the cursor for the `CRDTree` will
change
-}
applyLocal : Operation a -> CRDTree a -> Result Error (CRDTree a)
applyLocal operation (CRDTree record as tree) =
  let
      mapResult rid timestamp path result =
        case result of
          Err AlreadyApplied ->
            Ok <| CRDTree { record | lastOperation = Batch [] }

          Err TombstoneUpdate ->
            Ok <| CRDTree { record | lastOperation = Batch [] }

          Err exp ->
            Err <|
              { replicaId = rid
              , timestamp = timestamp
              , path = path
              }

          Ok node ->
            let
                update =
                  updateTimestamp rid timestamp
                    >> appendOperation operation
                    >> updateCursor timestamp path
            in
                Ok <| update <| CRDTree { record | root = node }
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
          applyBatch (List.map apply ops) tree


applyBatch funcs (CRDTree record as tree) =
  batchFold tree funcs
    (Ok <| CRDTree { record | lastOperation = Batch [] })


batchFold : CRDTree a -> List (CRDTree a -> Result Error (CRDTree a))
                      -> Result Error (CRDTree a)
                      -> Result Error (CRDTree a)
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
           -> Result CRDTree.List.Error (List (Node a))
addFun value path maybePreviousTs nodes =
  let
      node =
        Node.init value path

      timestamp =
        Node.timestamp node

  in
      case maybePreviousTs of
        Just previousTs ->
          insertWhen (\n -> (Node.timestamp n) == previousTs) node nodes

        Nothing ->
          case find (\n -> (Node.timestamp n) == timestamp) nodes of
            Just _ ->
              Err AlreadyApplied

            Nothing ->
              Ok [ node ]


deleteFun : List Int -> Maybe Int
                     -> List (Node a)
                     -> Result CRDTree.List.Error (List (Node a))
deleteFun path maybePreviousTs nodes =
  case maybePreviousTs of
    Just previousTs ->
      let
          node = Node.tombstone path
          pred = (\n -> (Node.timestamp n) == previousTs)
      in
          replaceWhen pred node nodes

    Nothing ->
      Err NotFound


updateBranch : UpdateFun a -> List Int
                           -> Node a
                           -> Result CRDTree.List.Error (Node a)
updateBranch fun path parent =
  if Node.isDeleted parent then
    Err TombstoneUpdate
  else
    updateBranchHelp fun path parent <| Node.children parent


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
          applyWhen (\n -> (Node.timestamp n) == ts) update children
            |> updateChildren parent


updateChildren : Node a -> Result CRDTree.List.Error (List (Node a))
                        -> Result CRDTree.List.Error (Node a)
updateChildren parent result =
  Result.map (\children -> Node.updateChildren children parent) result


branchCursor : CRDTree a -> CRDTree a
branchCursor (CRDTree record) =
  CRDTree { record | cursor = record.cursor ++ [0] }


mergeLastOperation : CRDTree a -> CRDTree a -> CRDTree a
mergeLastOperation (CRDTree record1) (CRDTree record2) =
  let
      operations1 = record1.lastOperation
      operations2 = record2.lastOperation
      operation   = Operation.merge operations1 operations2
  in
    CRDTree { record2 | lastOperation = operation }


updateCursor : Int -> List Int -> CRDTree a -> CRDTree a
updateCursor timestamp path (CRDTree record) =
  CRDTree { record | cursor = buildPath timestamp path }


appendOperation : Operation a -> CRDTree a -> CRDTree a
appendOperation operation (CRDTree record) =
  CRDTree
    { record | operations = operation :: record.operations
    , lastOperation = operation
    }


updateTimestamp : ReplicaId -> Int -> CRDTree a -> CRDTree a
updateTimestamp rid operationTimestamp (CRDTree record as tree) =
  let
      timestamp =
        mergeTimestamp tree record.timestamp operationTimestamp

      replicaId =
        ReplicaId.toInt rid
  in
      CRDTree
        { record | timestamp = timestamp
        , replicas = Dict.insert replicaId operationTimestamp record.replicas
        }


mergeTimestamp : CRDTree a -> Int -> Int -> Int
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
nextTimestamp : CRDTree a -> Int -> Int
nextTimestamp (CRDTree record) timestamp =
  timestamp +
    (if record.maxReplicas < 2 then 1 else record.maxReplicas - 1)


{-| Return the last successfully applied operation
-}
lastOperation : CRDTree a -> Operation a
lastOperation (CRDTree record) =
  record.lastOperation


{-| The local replica id
-}
id : CRDTree a -> ReplicaId
id (CRDTree record) =
  record.replicaId


{-| Return a list of operations after a known timestamp
-}
operationsSince : Int -> CRDTree a -> List (Operation a)
operationsSince timestamp (CRDTree record) =
  case timestamp of
    0 ->
      record.operations |> List.reverse

    _ ->
      Operation.since timestamp record.operations


{-| Get a value at path
-}
root : CRDTree a -> Node a
root (CRDTree record) =
  record.root


{-| Get a value at path
-}
get : List Int -> CRDTree a -> Maybe a
get path (CRDTree record) =
  Node.descendant path record.root |> Maybe.andThen Node.value


{-| Return the tree cursor
-}
cursor : CRDTree a -> List Int
cursor (CRDTree record) =
  record.cursor


{-| Move the tree cursor to the nearest branch above
-}
moveCursorUp : CRDTree a -> CRDTree a
moveCursorUp (CRDTree record as tree) =
  let
      newCursor =
        List.head (cursor tree)
        |> Maybe.withDefault 0
        |> List.singleton
  in
      CRDTree { record | cursor = newCursor }


buildPath : Int -> List Int -> List Int
buildPath timestamp path =
  case List.reverse path of
    [] ->
      [ timestamp ]

    _ :: rest ->
      List.reverse <| timestamp :: rest

