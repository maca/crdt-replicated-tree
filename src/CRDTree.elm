module CRDTree exposing
    ( CRDTree
    , init
    , Error(..)
    , add
    , addAfter
    , addBranch
    , batch
    , delete
    , apply
    , operationsSince
    , lastOperation
    , id
    , timestamp
    , root
    , get
    , getValue
    , cursor
    , moveCursorUp
    , lastReplicaTimestamp
    )

{-| The local replica state.

Not two replicas should have the same id across a network, a server
should be used to assign unique numeric ids to each replica.

The timestamp for the replica is a vector clock, encoded as an integer,
consisting in the id of the replica taking the first bits and the
number of the last operation taking the last 32 bits.

The path of a node in the tree is represented as a `List Int`.


# Init

@docs CRDTree
@docs init


# Operations

@docs Error
@docs add
@docs addAfter
@docs addBranch
@docs batch
@docs delete
@docs apply
@docs operationsSince
@docs lastOperation


# Tree

@docs id
@docs timestamp
@docs root
@docs get
@docs getValue
@docs cursor
@docs moveCursorUp


# Replicas

@docs lastReplicaTimestamp

-}

import Array exposing (Array)
import CRDTree.Node as Node exposing (Error(..), Node(..))
import CRDTree.Operation as Operation exposing (Operation(..))
import CRDTree.Timestamp as Timestamp exposing (replicaId)
import Dict exposing (Dict)
import Result


{-| Failure to apply an operation
-}
type Error a
    = Error (Operation a)


{-| A Replicated Tree, see [int](#init).
-}
type CRDTree a
    = CRDTree
        { root : Node a
        , timestamp : Int
        , cursor : Array Int
        , operations : List (Operation a)
        , replicas : Dict Int Int
        , lastOperation : Operation a
        }


{-| Build a CRDTree providing the replica id.

    tree : CRDTree String
    tree =
        init 1

-}
init : Int -> CRDTree a
init replicaId =
    CRDTree
        { operations = []
        , cursor = Array.fromList [ 0 ]
        , replicas = Dict.empty
        , root = Node.root
        , timestamp = Timestamp.init replicaId
        , lastOperation = Batch []
        }


{-| Add a node after tree cursor, the cursor is set
at the added node path.

    init 1
        |> add "a"
        |> Result.andThen (add "b")
        |> Result.andThen (add "c")

-}
add : a -> CRDTree a -> Result (Error a) (CRDTree a)
add value ((CRDTree record) as tree) =
    addAfter (record.cursor |> Array.toList) value tree


{-| Add a node after another node at given path,
the cursor is set at the added node path.

    init 1
      |> add "a"
      |> Result.andThen (add "b")
      |> Result.andThen (addAfter [1] "c")
    -- node with value "c" is inserted between nodes "a" and "b"

-}
addAfter : List Int -> a -> CRDTree a -> Result (Error a) (CRDTree a)
addAfter path value tree =
    applyLocal (Add (nextTimestamp tree) path value) tree


{-| Add a branch after tree cursor, subsequent
additions are added to the branch.

    init 1
        |> addBranch "a"
        |> Result.andThen (add "a,b")
        |> Result.andThen (add "a,c")

-}
addBranch : a -> CRDTree a -> Result (Error a) (CRDTree a)
addBranch value tree =
    add value tree
        |> Result.map
            (\(CRDTree rec) ->
                CRDTree { rec | cursor = Array.push 0 rec.cursor }
            )


{-| Delete a node at a path

    init 1
        |> batch [ add "a", add "b" ]
        |> Result.andThen (\tree -> delete (cursor tree) tree)

Nodes are not actually removed but marked as deleted and their
children discarded.

-}
delete : List Int -> CRDTree a -> Result (Error a) (CRDTree a)
delete path tree =
    applyLocal (Delete path) tree


{-| Apply a list of operations

    init 1 |> batch [ add "a", add "b", add "c" ]

-}
batch :
    List (CRDTree a -> Result (Error a) (CRDTree a))
    -> CRDTree a
    -> Result (Error a) (CRDTree a)
batch funcs (CRDTree record) =
    List.foldl
        (\f r -> Result.andThen (f >> Result.map2 mergeOperations r) r)
        (CRDTree { record | lastOperation = Batch [] } |> Ok)
        funcs


{-| Apply a remote operation

    treeA : CRDTree String
    treeA =
      let
          tree = init 1
      in
      tree
        |> batch [ add "a", add "b", add "c" ]
        |> Result.withDefault tree

    operation : Operation String
    operation =
      lastOperation treeA

    treeB : CRDTree String
    treeB =
      let
          tree = init 1
      in
      tree
        |> apply operation
        |> Result.withDefault tree

    (root treeA) == (root treeB)
    (operations treeA) == (operations treeB)
    (path treeA) /= (path treeB)
    (timestamp treeA) /= (timestamp treeB)

-}
apply : Operation a -> CRDTree a -> Result (Error a) (CRDTree a)
apply operation tree =
    applyLocal operation tree
        |> Result.map
            (\(CRDTree rec) -> CRDTree { rec | cursor = cursor tree })


{-| Apply a local operation, the cursor for the `CRDTree` will
change
-}
applyLocal : Operation a -> CRDTree a -> Result (Error a) (CRDTree a)
applyLocal operation ((CRDTree record) as tree) =
    case operation of
        Add ts path value ->
            let
                additionOp =
                    if id tree == replicaId ts then
                        Node.localAddAfter

                    else
                        Node.addAfter
            in
            record.root
                |> additionOp path ( ts, value )
                |> updateTree operation path ts tree
                |> Result.map (mergeTimestamp ts)

        Delete path ->
            let
                ts =
                    Operation.timestamp operation |> Maybe.withDefault 0
            in
            record.root
                |> Node.delete path
                |> updateTree operation path ts tree

        Batch ops ->
            batch (List.map apply ops) tree


updateTree :
    Operation a
    -> List Int
    -> Int
    -> CRDTree a
    -> Result Node.Error (Node a)
    -> Result (Error a) (CRDTree a)
updateTree operation path ts (CRDTree rec) result =
    case result of
        Ok node ->
            { rec
                | root = node
                , cursor = buildPath ts path
                , operations = operation :: rec.operations
                , lastOperation = operation
                , replicas = Dict.insert (replicaId ts) ts rec.replicas
            }
                |> CRDTree
                |> Ok

        Err AlreadyApplied ->
            CRDTree { rec | lastOperation = Batch [] } |> Ok

        Err _ ->
            Error operation |> Err


mergeOperations : CRDTree a -> CRDTree a -> CRDTree a
mergeOperations (CRDTree one) (CRDTree two) =
    let
        operation =
            Operation.merge one.lastOperation two.lastOperation
    in
    CRDTree { two | lastOperation = operation }


mergeTimestamp : Int -> CRDTree a -> CRDTree a
mergeTimestamp ts (CRDTree record) =
    let
        ts2 =
            Timestamp.incrementTo ts record.timestamp
    in
    CRDTree { record | timestamp = ts2 }


{-| Get the next timestamp
-}
nextTimestamp : CRDTree a -> Int
nextTimestamp (CRDTree record) =
    Timestamp.increment record.timestamp


{-| Return the last successfully applied operation or batch
or if operation was not succesfull an empty batch.

      import Operation exposing (Operation(..))

      -- success
      init 1
        |> batch [ add "a", add "b", add "c" ]
        |> Result.map (\tree ->
             (lastOperation tree) /= Batch [])

      -- failure
      init 1
        |> delete [1,2,3]
        |> Result.map (\tree ->
            (lastOperation tree) == Batch [])

-}
lastOperation : CRDTree a -> Operation a
lastOperation (CRDTree record) =
    record.lastOperation


{-| The local replica id
-}
id : CRDTree a -> Int
id tree =
    timestamp tree |> Timestamp.replicaId


{-| The local replica timestamp
-}
timestamp : CRDTree a -> Int
timestamp (CRDTree record) =
    record.timestamp


{-| Return a batch of operations after a known timestamp

    treeA : CRDTree String
    treeA =
      let
          tree = init 1
      in
      tree
        |> batch [ add "a", add "b" ]
        |> Result.withDefault tree

    (List.length (Operation.toList <| operationsSince 0 treeA)) == 2
    (List.length (Operation.toList <| operationsSince 1 treeA)) == 2
    (List.length (Operation.toList <| operationsSince 2 treeA)) == 1

    (List.length (Operation.toList <| operationsSince 5 treeA)) == 0

-}
operationsSince : Int -> CRDTree a -> Operation a
operationsSince initalTimestamp (CRDTree record) =
    case initalTimestamp of
        0 ->
            record.operations
                |> List.reverse
                |> Operation.fromList

        _ ->
            Operation.since initalTimestamp record.operations
                |> Operation.fromList


{-| Root node of the CRDTree
-}
root : CRDTree a -> Node a
root (CRDTree record) =
    record.root


{-| Get a value at path

    treeA : CRDTree String
    treeA =
      let
          tree = init 1
      in
      tree
        |> batch [ addBranch "a", addBranch "b", add "c" ]
        |> Result.withDefault tree

    (get [1] treeA) == (Just (Node.init "a" [1]))
    (get [1, 2] treeA) == (Just (Node.init "b" [1,2]))
    (get [1, 2, 3] treeA) == (Just (Node.init "c" [1, 2, 3]))
    (get [4] treeA) == Nothing

-}
get : List Int -> CRDTree a -> Maybe (Node a)
get path (CRDTree record) =
    Node.descendant path record.root


{-| Get a value at path

    treeA : CRDTree String
    treeA =
      let
          tree = init 1
      in
      tree
        |> batch [ addBranch "a", addBranch "b", add "c" ]
        |> Result.withDefault tree

    (get [1] treeA) == (Just "a")
    (get [1, 2] treeA) == (Just "b")
    (get [1, 2, 3] treeA) == (Just "c")
    (get [4] treeA) == Nothing

-}
getValue : List Int -> CRDTree a -> Maybe a
getValue path tree =
    get path tree |> Maybe.andThen Node.value


{-| Return the tree cursor

    treeA : CRDTree String
    treeA =
      let
          tree = init 1
      in
      tree
        |> batch [ add "a", add "b", add "c" ]
        |> Result.withDefault tree

    (cursor treeA) == [3]


    treeB : CRDTree String
    treeB =
      let
          tree = init 1
      in
      tree
        |> batch [ addBranch "a", addBranch "b" ]
        |> Result.withDefault tree

    (cursor treeB) == [1, 2, 0]

-}
cursor : CRDTree a -> Array Int
cursor (CRDTree record) =
    record.cursor


{-| Move the tree cursor one level up

    treeA : CRDTree String
    treeA =
      let
          tree = init 1
      in
      tree
        |> batch [ addBranch "a", addBranch "b", add "c" ]
        |> Result.withDefault tree

    (cursor treeA) == [1, 2, 3]
    (cursor (moveCursorUp treeA)) == [1, 2]

-}
moveCursorUp : CRDTree a -> CRDTree a
moveCursorUp ((CRDTree record) as tree) =
    if (Array.length <| cursor tree) == 1 then
        tree

    else
        CRDTree { record | cursor = cursor tree |> Array.slice 0 -1 }


buildPath : Int -> List Int -> Array Int
buildPath ts path =
    Array.fromList path
        |> Array.slice 0 -1
        |> Array.push ts


{-| Last know timestamp for a replica
-}
lastReplicaTimestamp : Int -> CRDTree a -> Int
lastReplicaTimestamp replicaId (CRDTree { replicas }) =
    Dict.get replicaId replicas |> Maybe.withDefault 0
