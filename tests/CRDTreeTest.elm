module CRDTreeTest exposing (..)

import CRDTree
    exposing
        ( CRDTree
        , add
        , addAfter
        , addBranch
        , apply
        , batch
        , delete
        , get
        , getValue
        , lastOperation
        , operationsSince
        )
import CRDTree.Node as Node exposing (Node)
import CRDTree.Operation as Operation exposing (Operation(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "CRDTree"
        [ describe "add"
            [ test "apply Add succeeds" <|
                \_ ->
                    add "a" tree |> Expect.ok

            --
            , test "apply Add result updates tree nodes" <|
                \_ ->
                    add "a" tree
                        |> Result.withDefault tree
                        |> getValue [ 0x00100000 ]
                        |> Expect.equal (Just "a")

            --
            , test "apply Add sets tree operations" <|
                \_ ->
                    add "a" tree
                        |> Result.withDefault tree
                        |> operationsSince 0
                        |> Expect.equal
                            (Batch [ addFirstOperation ])

            --
            , test "sets last operation" <|
                \_ ->
                    add "a" tree
                        |> Result.withDefault tree
                        |> lastOperation
                        |> Expect.equal
                            addFirstOperation
            ]

        --
        , test "add after" <|
            \_ ->
                add "a" tree
                    |> Result.andThen (add "b")
                    |> Result.andThen (addAfter [ 0x00100000 ] "c")
                    |> Result.withDefault tree
                    |> CRDTree.root
                    |> Node.filterMap Node.value
                    |> Expect.equal [ "a", "c", "b" ]

        --
        , test "add branch" <|
            \_ ->
                addBranchResult
                    |> Result.withDefault tree
                    |> CRDTree.root
                    |> Node.map iter3
                    |> Expect.equal
                        [ ( "a"
                          , [ ( "b"
                              , [ ( "c"
                                  , [ "d", "e" ]
                                  )
                                ]
                              )
                            ]
                          )
                        ]

        --
        , test "delete" <|
            \_ ->
                deleteResult
                    |> Result.withDefault tree
                    |> CRDTree.root
                    |> Node.filterMap Node.value
                    |> Expect.equal [ "b" ]

        --
        , test "add to deleted branch" <|
            \_ ->
                addToDeletedBranchResult
                    |> Result.withDefault tree
                    |> CRDTree.root
                    |> Node.map iter3
                    |> Expect.equal
                        [ ( "a", [ ( "b", [] ) ] ) ]

        --
        , describe "batch"
            [ test "apply Batch succeeds" <|
                \_ ->
                    batchResult
                        |> Result.withDefault tree
                        |> CRDTree.root
                        |> Node.filterMap Node.value
                        |> Expect.equal [ "a", "b" ]

            --
            , test "apply Batch sets tree operations" <|
                \_ ->
                    batchResult
                        |> Result.withDefault tree
                        |> operationsSince 0
                        |> Expect.equal batchResultOperations

            --
            , test "sets last operation" <|
                \_ ->
                    batchResult
                        |> Result.withDefault tree
                        |> lastOperation
                        |> Expect.equal batchResultOperations
            ]

        --
        , describe "applies several remote operations"
            [ test "apply Batch succeeds" <|
                \_ ->
                    Expect.ok applyResult

            --
            , test "apply Batch adds first node" <|
                \_ ->
                    applyResult
                        |> Result.withDefault tree
                        |> getValue [ 1 ]
                        |> Expect.equal (Just "a")

            --
            , test "apply Batch adds second node" <|
                \_ ->
                    applyResult
                        |> Result.withDefault tree
                        |> getValue [ 2 ]
                        |> Expect.equal (Just "b")

            --
            , test "apply Batch sets tree operations" <|
                \_ ->
                    applyResult
                        |> Result.withDefault tree
                        |> operationsSince 0
                        |> Expect.equal applyResultBatch

            --
            , test "sets last operation" <|
                \_ ->
                    applyResult
                        |> Result.withDefault tree
                        |> lastOperation
                        |> Expect.equal applyResultBatch
            ]

        --
        , test "batch atomicity" <|
            \_ ->
                let
                    batch =
                        Batch
                            [ Add 1 [ 0 ] "a"
                            , Add 2 [ 9 ] "b"
                            ]

                    result =
                        apply batch tree
                in
                Expect.err result

        --
        , describe "applying Add multiple times is the same as once"
            [ test "apply Add multiple times succeeds" <|
                always (Expect.ok idempotencyResult)

            --
            , test "apply Add multiple times" <|
                \_ ->
                    idempotencyResult
                        |> Result.withDefault tree
                        |> CRDTree.root
                        |> Node.map iter3
                        |> Expect.equal
                            [ ( "a", [] ) ]

            --
            , test "tree operations" <|
                \_ ->
                    idempotencyResult
                        |> Result.withDefault tree
                        |> operationsSince 0
                        |> Expect.equal (Batch [ Add 1 [ 0 ] "a" ])

            --
            , test "last operation" <|
                \_ ->
                    idempotencyResult
                        |> Result.withDefault tree
                        |> lastOperation
                        |> Expect.equal (Batch [ Add 1 [ 0 ] "a" ])
            ]

        --
        , describe "operations since"
            [ test "since beginning" <|
                \_ ->
                    operationsSinceResult
                        |> Result.withDefault tree
                        |> operationsSince 0
                        |> Expect.equal operationsSinceBatch

            --
            , test "since arbitrary timestamp" <|
                \_ ->
                    operationsSinceResult
                        |> Result.withDefault tree
                        |> operationsSince 2
                        |> Expect.equal
                            (Batch
                                [ Add 2 [ 1 ] "b"
                                , Add 3 [ 2 ] "c"
                                , Add 4 [ 3 ] "d"
                                , Delete [ 3 ]
                                , Add 5 [ 4 ] "e"
                                , Add 6 [ 5 ] "f"
                                ]
                            )

            --
            , test "since last" <|
                \_ ->
                    operationsSinceResult
                        |> Result.withDefault tree
                        |> operationsSince 6
                        |> Expect.equal
                            (Batch
                                [ Add 6 [ 5 ] "f" ]
                            )

            --
            , test "not present returns empty" <|
                \_ ->
                    operationsSinceResult
                        |> Result.withDefault tree
                        |> operationsSince 10
                        |> Expect.equal
                            (Batch [])
            ]
        ]


tree =
    CRDTree.init 0


addFirstOperation =
    Add 0x00100000 [ 0 ] "a"


addBranchResult =
    tree
        |> batch
            [ addBranch "a"
            , addBranch "b"
            , addBranch "c"
            , add "d"
            , add "e"
            ]


deleteResult =
    add "a" tree
        |> Result.andThen (add "b")
        |> Result.andThen (delete [ 0x00100000 ])


addToDeletedBranchResult =
    addBranchResult
        |> Result.andThen
            (delete [ 0x00100000, 0x00200000, 0x00300000 ])
        |> Result.andThen
            (addAfter
                [ 0x00100000, 0x00200000, 0x00300000, 0x00400000 ]
                "x"
            )


batchResult =
    batch [ add "a", add "b" ] tree


batchResultOperations =
    Batch
        [ Add 0x00100000 [ 0 ] "a"
        , Add 0x00200000 [ 0x00100000 ] "b"
        ]


applyResult =
    apply applyResultBatch tree


applyResultBatch =
    Batch
        [ Add 1 [ 0 ] "a"
        , Add 2 [ 1 ] "b"
        ]


idempotencyResult =
    apply idempotencyBatch tree


idempotencyBatch =
    Batch
        [ Add 1 [ 0 ] "a"
        , Add 1 [ 0 ] "a"
        , Add 1 [ 0 ] "a"
        , Add 1 [ 0 ] "a"
        ]


iter fn n =
    ( Node.value n |> Maybe.withDefault "", fn n )


iter3 =
    iter
        (Node.map
            (iter
                (Node.map
                    (iter
                        (Node.filterMap Node.value)
                    )
                )
            )
        )


operationsSinceResult =
    apply operationsSinceBatch tree


operationsSinceBatch =
    Batch
        [ Add 1 [ 0 ] "a"
        , Add 2 [ 1 ] "b"
        , Add 3 [ 2 ] "c"
        , Add 4 [ 3 ] "d"
        , Delete [ 3 ]
        , Add 5 [ 4 ] "e"
        , Add 6 [ 5 ] "f"
        ]
