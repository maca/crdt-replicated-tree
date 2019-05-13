module RGTest exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Test exposing (..)
import List exposing (map, reverse)

import RG exposing
  ( batch
  -- , delete
  -- , addBranch
  , add
  , apply
  )
import RG.Node as Node exposing
  ( Node
  , tombstone
  )
import RG.Operation as Operation exposing
  ( Operation(..)
  , ReplicaId(..)
  )

type Data = Data

data = Just Data


suite : Test
suite = describe "RG"
  [ testAdd
    "applies Add operation"

  , testBatch
    "performs several operations"

  , testApplyBatch
    "applies several remote operations"

  , testBatchAtomicity
    "batch fails if an operation fails"

  , testAddIsIdempotent
    "applying Add multiple times is the same as once"

  , testInsertionBetweenNodes
    "apply Add inserts at any position"

  , testAddLeaf
    "inserts node as children of nested branch"

  , testDelete
    "Delete marks node as tombstone"

  , testDeleteIsIdempotent
    "apply same Delete operation yields same results"

  , testOperationsSince
    "gets operations since a timestamp"

  --   -- , test "apply Delete locally sets pointer" <| \_ ->
  -- , describe "replicas"
  -- , describe "local operations"
  ]

testAdd description =
  let
      rga =
        RG.init { replicaId = 0, maxReplicas = 1 }

      operation =
        Add rga.replicaId 1 [-1, 0] data

      result =
        add data rga
  in
      describe description
        [ test "apply Add succeeds"
          <| always (Expect.ok result)

        , test "apply Add result updates rga nodes" <| \_ ->
          expectNode [-1, 1] (Node.init data [-1, 1]) result

        , test "apply Add increments timestamp"
          <| always (expectTimestamp 1 result)

        , test "sets rga pointer"
          <| always (expectPointer [-1, 1] result)

        , test "apply Add sets rga operations" <| \_ ->
          let
              operations =
                  [ Add rga.replicaId 1 [-1, 0] data
                  , Add rga.replicaId -1 [0] Nothing
                  ]
          in
              expectOperations operations result

        , test "sets last operation"
          <| always (expectLastOperation operation result)
        ]


testBatch description =
  let
      rga =
        RG.init { replicaId = 0, maxReplicas = 1 }

      result =
        batch [add data, add data] rga
  in
      describe description
        [ test "apply Batch succeeds"
          <| always (Expect.ok result)

        , test "apply Batch adds first node" <| \_ ->
          expectNode [-1, 1] (Node.init data [-1, 1]) result

        , test "apply Batch adds second node" <| \_ ->
          expectNode [-1, 2] (Node.init data [-1, 2]) result

        , test "apply Batch increments timestamp"
          <| always (expectTimestamp 2 result)

        , test "apply Batch sets rga operations" <| \_ ->
          let
              operations =
                [ Add rga.replicaId 2 [-1, 1] data
                , Add rga.replicaId 1 [-1, 0] data
                , Add rga.replicaId -1 [0] Nothing
                ]
          in
              expectOperations operations result

        , test "sets last operation" <| \_ ->
          let
              operation = Batch
                [ Add rga.replicaId 1 [-1, 0] data
                , Add rga.replicaId 2 [-1, 1] data
                ]
          in
              expectLastOperation operation result
        ]


testApplyBatch description =
  let
      rga =
        RG.init { replicaId = 0, maxReplicas = 1 }

      batch = Batch
        [ Add rga.replicaId 1 [-1] data
        , Add rga.replicaId 2 [1] data
        ]

      result =
        apply batch rga
  in
      describe description
        [ test "apply Batch succeeds"
          <| always (Expect.ok result)

        , test "apply Batch adds first node" <| \_ ->
          expectNode [1] (Node.init data [1]) result

        , test "apply Batch adds second node" <| \_ ->
          expectNode [2] (Node.init data [2]) result

        , test "apply Batch increments timestamp"
          <| always (expectTimestamp 2 result)

        , test "apply Batch sets rga operations" <| \_ ->
          let
              operations =
                [ Add rga.replicaId 2 [1] data
                , Add rga.replicaId 1 [-1] data
                , Add rga.replicaId -1 [0] Nothing
                ]
          in
              expectOperations operations result

        , test "sets last operation"
          <| always (expectLastOperation batch result)
        ]


testAddIsIdempotent description =
  let
      rga =
        RG.init { replicaId = 0, maxReplicas = 1 }

      batch = Batch
        [ Add rga.replicaId 1 [-1] data
        , Add rga.replicaId 1 [-1] data
        , Add rga.replicaId 1 [-1] data
        , Add rga.replicaId 1 [-1] data
        ]

      result =
        apply batch rga
  in
      describe description

        [ test "apply Add multiple times succeeds"
          <| always (Expect.ok result)

        , test "apply Add multiple times result updates rga nodes" <| \_ ->
          expectNode [1] (Node.init data [1]) result

        , test "apply Add multiple times increments timestamp"
          <| always (expectTimestamp 1 result)

        , test "apply Add multiple times sets rga operations" <| \_ ->
          let
              operations =
                [ Add rga.replicaId 1 [-1] data
                , Add rga.replicaId -1 [0] Nothing
                ]
          in
              expectOperations operations result

        , test "sets last operation" <| \_ ->
          let
              operation = Batch
                [ Add rga.replicaId 1 [-1] data ]
          in
              expectLastOperation operation result
        ]


testInsertionBetweenNodes _ =
  let
      rga =
        RG.init { replicaId = 0, maxReplicas = 1 }

      batch = Batch
        [ Add rga.replicaId 1 [-1] data
        , Add rga.replicaId 2 [1] data
        , Add rga.replicaId 3 [1] data
        ]

      result =
        apply batch rga
  in
      describe "apply Add inserts between nodes"
        [ test "apply Add insert succeeds"
          <| always (Expect.ok result)

        , test "apply Add insert adds first node" <| \_ ->
          expectNode [1] (Node.init data [1]) result

        , test "apply Add insert adds second node" <| \_ ->
          expectNode [2] (Node.init data [2]) result

        , test "apply Add insert adds third node" <| \_ ->
          expectNode [3] (Node.init data [3]) result

        , test "apply Add insert increments timestamp"
          <| always (expectTimestamp 3 result)

        , test "apply Add insert sets rga operations" <| \_ ->
          let
              operations =
                  [ Add rga.replicaId 3 [1] data
                  , Add rga.replicaId 2 [1] data
                  , Add rga.replicaId 1 [-1] data
                  , Add rga.replicaId -1 [0] Nothing
                  ]
          in
              expectOperations operations result

        , test "sets last operation"
          <| always (expectLastOperation batch result)
        ]


testAddLeaf description =
  let
      rga =
        RG.init { replicaId = 0, maxReplicas = 1 }

      batch = Batch
        [ Add rga.replicaId 1 [-1] data
        , Add rga.replicaId 2 [1, 0] data
        , Add rga.replicaId 3 [1, 2] data
        ]

      result =
        apply batch rga
  in
      describe description
        [ test "apply Add leaf succeeds"
          <| always (Expect.ok result)

        , test "apply Add leaf adds first leaf" <| \_ ->
          expectNode [1, 2] (Node.init data [1, 2]) result

        , test "apply Add leaf adds second leaf" <| \_ ->
          expectNode [1, 3] (Node.init data [1, 3]) result

        ,test "apply Add leaf increments timestamp"
          <| always (expectTimestamp 3 result)

        , test "apply Add leaf sets rga operations" <| \_ ->
          let
              operations =
                  [ Add rga.replicaId 3 [1, 2] data
                  , Add rga.replicaId 2 [1, 0] data
                  , Add rga.replicaId 1 [-1] data
                  , Add rga.replicaId -1 [0] Nothing
                  ]
          in
              expectOperations operations result

        , test "sets last operation"
          <| always (expectLastOperation batch result)
        ]


testBatchAtomicity description =
  test description <| \_ ->
    let
        rga =
          RG.init { replicaId = 0, maxReplicas = 1 }

        batch = Batch
          [ Add rga.replicaId 1 [0] data
          , Add rga.replicaId 2 [9] data
          ]

        result =
          apply batch rga
    in
        Expect.err result


testDelete description =
  let
      rga =
        RG.init { replicaId = 0, maxReplicas = 1 }

      batch = Batch
        [ Add rga.replicaId 1 [-1] data
        , Delete rga.replicaId [1]
        ]

      result =
        apply batch rga
  in
      describe description
        [ test "apply Add delete succeeds"
          <| always (Expect.ok result)

        , test "apply Delete doesn't increment timestamp"
          <| always (expectTimestamp 1 result)

        , test "apply Delete result updates rga nodes" <| \_ ->
          expectNode [1] (tombstone [1]) result

        , test "sets last operation"
          <| always (expectLastOperation batch result)
        ]


testDeleteIsIdempotent description =
  let
      rga =
        RG.init { replicaId = 0, maxReplicas = 1 }

      batch = Batch
        [ Add rga.replicaId 1 [-1] data
        , Delete rga.replicaId [1]
        , Delete rga.replicaId [1]
        , Delete rga.replicaId [1]
        , Delete rga.replicaId [1]
        , Delete rga.replicaId [1]
        ]

      result =
        apply batch rga
  in
      describe description

        [ test "apply Add multiple times succeeds"
          <| always (Expect.ok result)

        , test "apply Add multiple times result updates rga nodes" <| \_ ->
          expectNode [1] (tombstone [1]) result

        , test "apply Add multiple times increments timestamp"
          <| always (expectTimestamp 1 result)

        , test "apply Add multiple times sets rga operations" <| \_ ->
          let
              operations =
                [ Delete rga.replicaId [1]
                , Add rga.replicaId 1 [-1] data
                , Add rga.replicaId -1 [0] Nothing
                ]
          in
              expectOperations operations result

        , test "sets last operation" <| \_ ->
          let
              operation = Batch
                [ Add rga.replicaId 1 [-1] data
                , Delete rga.replicaId [1]
                ]
          in
              expectLastOperation operation result
        ]


testOperationsSince description =
  let
      rga_ =
        RG.init { replicaId = 0, maxReplicas = 1 }

      batch = Batch
        [ Add rga_.replicaId 1 [-1] data
        , Add rga_.replicaId 2 [1] data
        , Add rga_.replicaId 3 [2] data
        , Add rga_.replicaId 4 [3] data
        , Delete rga_.replicaId [3]
        , Batch []
        , Add rga_.replicaId 5 [4] data
        , Add rga_.replicaId 6 [5] data
        ]

      rga =
        apply batch rga_ |> Result.withDefault rga_
  in
      describe description
        [ test "operations since beginning" <| \_ ->
          let
              operations =
                [ Add rga_.replicaId -1 [0] Nothing
                , Add rga_.replicaId 1 [-1] data
                , Add rga_.replicaId 2 [1] data
                , Add rga_.replicaId 3 [2] data
                , Add rga_.replicaId 4 [3] data
                , Delete rga_.replicaId [3]
                , Add rga_.replicaId 5 [4] data
                , Add rga_.replicaId 6 [5] data
                ]
          in
              Expect.equal operations
                <| Operation.sinceTimestamp -1 rga.operations

        , test "operations since 2" <| \_ ->
          let
              operations =
                [ Add rga_.replicaId 2 [1] data
                , Add rga_.replicaId 3 [2] data
                , Add rga_.replicaId 4 [3] data
                , Delete rga_.replicaId [3]
                , Add rga_.replicaId 5 [4] data
                , Add rga_.replicaId 6 [5] data
                ]
          in
              Expect.equal operations
                <| Operation.sinceTimestamp 2 rga.operations

        , test "operations since last" <| \_ ->
          let
              operations =
                [ Add rga_.replicaId 6 [5] data ]
          in
              Expect.equal operations
                <| Operation.sinceTimestamp 6 rga.operations

        , test "not present returns empty" <| \_ ->
          Expect.equal []
            <| Operation.sinceTimestamp 10 rga.operations
        ]


expectNode path exp result =
  expect (\rga ->
    Expect.equal (Just exp) (Node.descendant path rga.root)) result


expectTimestamp exp result =
  expect (\rga ->
    Expect.true
      ("expected RG `" ++ Debug.toString rga ++
      "` to have timestamp " ++ Debug.toString exp)
    (exp == rga.timestamp)) result


expectOperations exp result =
  expect (\rga -> Expect.equal exp rga.operations) result


expectLastOperation exp result =
  expect (\rga -> Expect.equal exp rga.lastOperation) result


expectPointer exp result =
  expect (\rga -> Expect.equal exp rga.pointer) result


expect fun result =
  Result.map fun result
    |> Result.withDefault (Expect.fail "failed")


