module RGTest exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Test exposing (..)
import List exposing (map, reverse)

import RG exposing
  ( RG(..)
  , batch
  , delete
  , addBranch
  , add
  , apply
  , lastOperation
  , operationsSince
  , getNode
  )
import RG.Node as Node exposing (Node, tombstone)
import RG.Operation as Operation exposing (Operation(..))
import RG.ReplicaId as ReplicaId exposing (ReplicaId)

type Data = Data

data = Just Data


suite : Test
suite = describe "RG"
  [ testAdd "adds node"

  , testAddBranch
    "adds branch"

  , testAddToDeletedBranch "attempts to add to deleted branch"

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
  ]

testAdd description =
  let
      replicaId = ReplicaId.fromInt 0

      graph =
        RG.init { id = ReplicaId.toInt replicaId, maxReplicas = 1 }

      operation =
        Add replicaId 1 [-1, 0] data

      result =
        add data graph
  in
      describe description
        [ test "apply Add succeeds"
          <| always (Expect.ok result)

        , test "apply Add result updates graph nodes" <| \_ ->
          expectNode [-1, 1] (Node.init data [-1, 1]) result

        , test "apply Add sets graph operations" <| \_ ->
          let
              operations =
                  [ Add replicaId -1 [0] Nothing
                  , Add replicaId 1 [-1, 0] data
                  ]
          in
              expectOperations operations result

        , test "sets last operation"
          <| always (expectLastOperation operation result)
        ]


testBatch description =
  let
      replicaId = ReplicaId.fromInt 0

      graph =
        RG.init { id = ReplicaId.toInt replicaId, maxReplicas = 1 }

      result =
        batch [add data, add data] graph
  in
      describe description
        [ test "apply Batch succeeds"
          <| always (Expect.ok result)

        , test "apply Batch adds first node" <| \_ ->
          expectNode [-1, 1] (Node.init data [-1, 1]) result

        , test "apply Batch adds second node" <| \_ ->
          expectNode [-1, 2] (Node.init data [-1, 2]) result

        , test "apply Batch sets graph operations" <| \_ ->
          let
              operations =
                [ Add replicaId -1 [0] Nothing
                , Add replicaId 1 [-1, 0] data
                , Add replicaId 2 [-1, 1] data
                ]
          in
              expectOperations operations result

        , test "sets last operation" <| \_ ->
          let
              operation = Batch
                [ Add replicaId 1 [-1, 0] data
                , Add replicaId 2 [-1, 1] data
                ]
          in
              expectLastOperation operation result
        ]


testAddBranch description =
  let
      replicaId = ReplicaId.fromInt 0

      graph =
        RG.init { id = ReplicaId.toInt replicaId, maxReplicas = 1 }

      result =
        batch [addBranch Nothing, add data] graph
  in
      describe description
        [ test "apply Batch succeeds"
          <| always (Expect.ok result)

        , test "apply Batch adds branch child" <| \_ ->
          expectNode [-1, 1, 2] (Node.init data [-1, 1, 2]) result

        , test "apply Batch sets graph operations" <| \_ ->
          let
              operations =
                [ Add replicaId -1 [0] Nothing
                , Add replicaId 1 [-1, 0] Nothing
                , Add replicaId 2 [-1, 1, 0] data
                ]
          in
              expectOperations operations result

        , test "sets last operation" <| \_ ->
          let
              operation = Batch
                [ Add replicaId 1 [-1, 0] Nothing
                , Add replicaId 2 [-1, 1, 0] data
                ]
          in
              expectLastOperation operation result
        ]


testAddToDeletedBranch description =
  let
      replicaId = ReplicaId.fromInt 0

      graph =
        RG.init { id = ReplicaId.toInt replicaId, maxReplicas = 1 }

      batch = Batch
        [ Add replicaId 1 [-1, 0] Nothing
        , Delete replicaId [-1, 1]
        , Add replicaId 2 [-1, 1, 0] data
        ]

      result =
        apply batch graph
  in
      describe description
        [ test "apply Batch succeeds"
          <| always (Expect.ok result)

        , test "apply Batch deletes branch" <| \_ ->
          expectNode [-1, 1] (Node.tombstone [-1, 1]) result

        , test "apply Batch sets graph operations" <| \_ ->
          let
              operations =
                [ Add replicaId -1 [0] Nothing
                , Add replicaId 1 [-1, 0] Nothing
                , Delete replicaId [-1, 1]
                ]
          in
              expectOperations operations result

        , test "sets last operation" <| \_ ->
          let
              operation = Batch
                [ Add replicaId 1 [-1, 0] Nothing
                , Delete replicaId [-1, 1]
                ]
          in
              expectLastOperation operation result
        ]


testApplyBatch description =
  let
      replicaId = ReplicaId.fromInt 0

      graph =
        RG.init { id = ReplicaId.toInt replicaId, maxReplicas = 1 }

      batch = Batch
        [ Add replicaId 1 [-1] data
        , Add replicaId 2 [1] data
        ]

      -- TODO: test concurrency by adding before and after batch

      result =
        apply batch graph
  in
      describe description
        [ test "apply Batch succeeds"
          <| always (Expect.ok result)

        , test "apply Batch adds first node" <| \_ ->
          expectNode [1] (Node.init data [1]) result

        , test "apply Batch adds second node" <| \_ ->
          expectNode [2] (Node.init data [2]) result

        , test "apply Batch sets graph operations" <| \_ ->
          let
              operations =
                [ Add replicaId -1 [0] Nothing
                , Add replicaId 1 [-1] data
                , Add replicaId 2 [1] data
                ]
          in
              expectOperations operations result

        , test "sets last operation"
          <| always (expectLastOperation batch result)
        ]


testAddIsIdempotent description =
  let
      replicaId = ReplicaId.fromInt 0

      graph =
        RG.init { id = ReplicaId.toInt replicaId, maxReplicas = 1 }

      batch = Batch
        [ Add replicaId 1 [-1] data
        , Add replicaId 1 [-1] data
        , Add replicaId 1 [-1] data
        , Add replicaId 1 [-1] data
        ]

      result =
        apply batch graph
  in
      describe description

        [ test "apply Add multiple times succeeds"
          <| always (Expect.ok result)

        , test "apply Add multiple times result updates graph nodes" <| \_ ->
          expectNode [1] (Node.init data [1]) result

        , test "apply Add multiple times sets graph operations" <| \_ ->
          let
              operations =
                [ Add replicaId -1 [0] Nothing
                , Add replicaId 1 [-1] data
                ]
          in
              expectOperations operations result

        , test "sets last operation" <| \_ ->
          let
              operation = Batch
                [ Add replicaId 1 [-1] data ]
          in
              expectLastOperation operation result
        ]


testInsertionBetweenNodes _ =
  let
      replicaId = ReplicaId.fromInt 0

      graph =
        RG.init { id = ReplicaId.toInt replicaId, maxReplicas = 1 }

      batch = Batch
        [ Add replicaId 1 [-1] data
        , Add replicaId 2 [1] data
        , Add replicaId 3 [1] data
        ]

      result =
        apply batch graph
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

        , test "apply Add insert sets graph operations" <| \_ ->
          let
              operations =
                  [ Add replicaId -1 [0] Nothing
                  , Add replicaId 1 [-1] data
                  , Add replicaId 2 [1] data
                  , Add replicaId 3 [1] data
                  ]
          in
              expectOperations operations result

        , test "sets last operation"
          <| always (expectLastOperation batch result)
        ]


testAddLeaf description =
  let
      replicaId = ReplicaId.fromInt 0

      graph =
        RG.init { id = ReplicaId.toInt replicaId, maxReplicas = 1 }

      batch = Batch
        [ Add replicaId 1 [-1] data
        , Add replicaId 2 [1, 0] data
        , Add replicaId 3 [1, 2] data
        ]

      result =
        apply batch graph
  in
      describe description
        [ test "apply Add leaf succeeds"
          <| always (Expect.ok result)

        , test "apply Add leaf adds first leaf" <| \_ ->
          expectNode [1, 2] (Node.init data [1, 2]) result

        , test "apply Add leaf adds second leaf" <| \_ ->
          expectNode [1, 3] (Node.init data [1, 3]) result

        , test "apply Add leaf sets graph operations" <| \_ ->
          let
              operations =
                  [ Add replicaId -1 [0] Nothing
                  , Add replicaId 1 [-1] data
                  , Add replicaId 2 [1, 0] data
                  , Add replicaId 3 [1, 2] data
                  ]
          in
              expectOperations operations result

        , test "sets last operation"
          <| always (expectLastOperation batch result)
        ]


testBatchAtomicity description =
  test description <| \_ ->
    let
        replicaId = ReplicaId.fromInt 0

        graph =
          RG.init { id = ReplicaId.toInt replicaId, maxReplicas = 1 }

        batch = Batch
          [ Add replicaId 1 [0] data
          , Add replicaId 2 [9] data
          ]

        result =
          apply batch graph
    in
        Expect.err result


testDelete description =
  let
      replicaId = ReplicaId.fromInt 0

      graph =
        RG.init { id = ReplicaId.toInt replicaId, maxReplicas = 1 }

      batch = Batch
        [ Add replicaId 1 [-1] data
        , Delete replicaId [1]
        ]

      result =
        apply batch graph
  in
      describe description
        [ test "apply Add delete succeeds"
          <| always (Expect.ok result)

        , test "apply Delete result updates graph nodes" <| \_ ->
          expectNode [1] (tombstone [1]) result

        , test "sets last operation"
          <| always (expectLastOperation batch result)
        ]


testDeleteIsIdempotent description =
  let
      replicaId = ReplicaId.fromInt 0

      graph =
        RG.init { id = ReplicaId.toInt replicaId, maxReplicas = 1 }

      batch = Batch
        [ Add replicaId 1 [-1] data
        , Delete replicaId [1]
        , Delete replicaId [1]
        , Delete replicaId [1]
        , Delete replicaId [1]
        , Delete replicaId [1]
        ]

      result =
        apply batch graph
  in
      describe description

        [ test "apply Add multiple times succeeds"
          <| always (Expect.ok result)

        , test "apply Add multiple times result updates graph nodes" <| \_ ->
          expectNode [1] (tombstone [1]) result

        , test "apply Add multiple times sets graph operations" <| \_ ->
          let
              operations =
                [ Add replicaId -1 [0] Nothing
                , Add replicaId 1 [-1] data
                , Delete replicaId [1]
                ]
          in
              expectOperations operations result

        , test "sets last operation" <| \_ ->
          let
              operation = Batch
                [ Add replicaId 1 [-1] data
                , Delete replicaId [1]
                ]
          in
              expectLastOperation operation result
        ]


testOperationsSince description =
  let
      replicaId =
        ReplicaId.fromInt 0

      graph2 =
        RG.init { id = ReplicaId.toInt replicaId, maxReplicas = 1 }

      batch = Batch
        [ Add replicaId 1 [-1] data
        , Add replicaId 2 [1] data
        , Add replicaId 3 [2] data
        , Add replicaId 4 [3] data
        , Delete replicaId [3]
        , Batch []
        , Add replicaId 5 [4] data
        , Add replicaId 6 [5] data
        ]

      graph =
        apply batch graph2 |> Result.withDefault graph2
  in
      describe description
        [ test "operations since beginning" <| \_ ->
          let
              operations =
                [ Add replicaId -1 [0] Nothing
                , Add replicaId 1 [-1] data
                , Add replicaId 2 [1] data
                , Add replicaId 3 [2] data
                , Add replicaId 4 [3] data
                , Delete replicaId [3]
                , Add replicaId 5 [4] data
                , Add replicaId 6 [5] data
                ]
          in
              Expect.equal operations
                <| operationsSince -1 graph

        , test "operations since 2" <| \_ ->
          let
              operations =
                [ Add replicaId 2 [1] data
                , Add replicaId 3 [2] data
                , Add replicaId 4 [3] data
                , Delete replicaId [3]
                , Add replicaId 5 [4] data
                , Add replicaId 6 [5] data
                ]
          in
              Expect.equal operations
                <| operationsSince 2 graph

        , test "operations since last" <| \_ ->
          let
              operations =
                [ Add replicaId 6 [5] data ]
          in
              Expect.equal operations
                <| operationsSince 6 graph

        , test "not present returns empty" <| \_ ->
          Expect.equal [] <| operationsSince 10 graph
        ]


expectNode path exp result =
  expect (\graph ->
    Expect.equal (Just exp) (getNode path graph)) result


expectOperations exp result =
  expect (\graph -> Expect.equal exp (operationsSince -1 graph)) result


expectLastOperation exp result =
  expect (\graph -> Expect.equal exp (lastOperation graph)) result


expect fun result =
  Result.map fun result
    |> Result.withDefault (Expect.fail "failed")


