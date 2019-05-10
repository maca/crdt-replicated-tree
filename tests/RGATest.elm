module RGATest exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Test exposing (..)
import List exposing (map, reverse)

import RGA exposing
  ( Operation(..)
  , RGA
  , ReplicaId
  -- , add
  -- , addBranch
  -- , delete
  -- , batch
  , apply
  , applyLocal
  , operationsSince
  )
import RGA.Node as Node exposing
  ( Node
  , node
  , tombstone
  )

type Data = Data

data = Just Data


suite : Test
suite = describe "RGA"
  [ describeAdd
    "applies Add operation"

  , describeBatch
    "performs several operations"

  , describeBatchAtomicity
    "batch fails if an operation fails"

  , describeAddIsIdempotent
    "applying Add multiple times is the same as once"

  , describeInsertionBetweenNodes
    "apply Add inserts at any position"

  , describeAddLeaf
    "inserts node as children of nested branch"

  , describeDelete
    "Delete marks node as tombstone"

  , describeDeleteIsIdempotent
    "apply same Delete operation yields same results"

  , describeOperationsSince
    "gets operations since a timestamp"

  --   -- , test "apply Delete locally sets pointer" <| \_ ->
  -- , describe "replicas"
  -- , describe "local operations"
  ]

describeAdd description =
  let
      rga =
        RGA.init { id = 0, maxReplicas = 1 }

      operation =
        Add rga.replicaId 1 [-1] data

      result =
        applyLocal operation rga
  in
      describe description
        [ test "apply Add succeeds"
          <| always (Expect.ok result)

        , test "apply Add result updates rga nodes" <| \_ ->
          expectRGANode [1] (node data 1 [1]) result

        , test "apply Add increments timestamp"
          <| always (expectRGATimestamp 1 result)

        , test "sets rga pointer"
          <| always (expectRGAPointer [1] result)

        , test "apply Add sets rga operations" <| \_ ->
          let
              operations =
                  [ Add rga.replicaId 1 [-1] data
                  , Add rga.replicaId -1 [0] Nothing
                  ]
          in
              expectRGAOperations operations result

        , test "sets last operation"
          <| always (expectLastOperation operation result)
        ]


describeBatch description =
  let
      rga =
        RGA.init { id = 0, maxReplicas = 1 }

      batch = Batch
        [ Add rga.replicaId 1 [-1] data
        , Add rga.replicaId 2 [1] data
        ]

      result =
        applyLocal batch rga
  in
      describe description
        [ test "apply Batch succeeds"
          <| always (Expect.ok result)

        , test "apply Batch adds first node" <| \_ ->
          expectRGANode [1] (node data 1 [1]) result

        , test "apply Batch adds second node" <| \_ ->
          expectRGANode [2] (node data 2 [2]) result

        , test "apply Batch increments timestamp"
          <| always (expectRGATimestamp 2 result)

        , test "apply Batch sets rga operations" <| \_ ->
          let
              operations =
                [ Add rga.replicaId 2 [1] data
                , Add rga.replicaId 1 [-1] data
                , Add rga.replicaId -1 [0] Nothing
                ]
          in
              expectRGAOperations operations result

        , test "sets last operation"
          <| always (expectLastOperation batch result)
        ]


describeAddIsIdempotent description =
  let
      rga =
        RGA.init { id = 0, maxReplicas = 1 }

      batch = Batch
        [ Add rga.replicaId 1 [-1] data
        , Add rga.replicaId 1 [-1] data
        , Add rga.replicaId 1 [-1] data
        , Add rga.replicaId 1 [-1] data
        ]

      result =
        applyLocal batch rga
  in
      describe description

        [ test "apply Add multiple times succeeds"
          <| always (Expect.ok result)

        , test "apply Add multiple times result updates rga nodes" <| \_ ->
          expectRGANode [1] (node data 1 [1]) result

        , test "apply Add multiple times increments timestamp"
          <| always (expectRGATimestamp 1 result)

        , test "apply Add multiple times sets rga operations" <| \_ ->
          let
              operations =
                [ Add rga.replicaId 1 [-1] data
                , Add rga.replicaId -1 [0] Nothing
                ]
          in
              expectRGAOperations operations result

        , test "sets last operation" <| \_ ->
          let
              operation = Batch
                [ Add rga.replicaId 1 [-1] data ]
          in
              expectLastOperation operation result
        ]


describeInsertionBetweenNodes _ =
  let
      rga =
        RGA.init { id = 0, maxReplicas = 1 }

      batch = Batch
        [ Add rga.replicaId 1 [-1] data
        , Add rga.replicaId 2 [1] data
        , Add rga.replicaId 3 [1] data
        ]

      result =
        applyLocal batch rga
  in
      describe "apply Add inserts between nodes"
        [ test "apply Add insert succeeds"
          <| always (Expect.ok result)

        , test "apply Add insert adds first node" <| \_ ->
          expectRGANode [1] (node data 1 [1]) result

        , test "apply Add insert adds second node" <| \_ ->
          expectRGANode [2] (node data 2 [2]) result

        , test "apply Add insert adds third node" <| \_ ->
          expectRGANode [3] (node data 3 [3]) result

        , test "apply Add insert increments timestamp"
          <| always (expectRGATimestamp 3 result)

        , test "apply Add insert sets rga operations" <| \_ ->
          let
              operations =
                  [ Add rga.replicaId 3 [1] data
                  , Add rga.replicaId 2 [1] data
                  , Add rga.replicaId 1 [-1] data
                  , Add rga.replicaId -1 [0] Nothing
                  ]
          in
              expectRGAOperations operations result

        , test "sets last operation"
          <| always (expectLastOperation batch result)
        ]


describeAddLeaf description =
  let
      rga =
        RGA.init { id = 0, maxReplicas = 1 }

      batch = Batch
        [ Add rga.replicaId 1 [-1] data
        , Add rga.replicaId 2 [1, 0] data
        , Add rga.replicaId 3 [1, 2] data
        ]

      result =
        applyLocal batch rga
  in
      describe description
        [ test "apply Add leaf succeeds"
          <| always (Expect.ok result)

        , test "apply Add leaf adds first leaf" <| \_ ->
          expectRGANode [1, 2] (node data 2 [1, 2]) result

        , test "apply Add leaf adds second leaf" <| \_ ->
          expectRGANode [1, 3] (node data 3 [1, 3]) result

        ,test "apply Add leaf increments timestamp"
          <| always (expectRGATimestamp 3 result)

        , test "apply Add leaf sets rga operations" <| \_ ->
          let
              operations =
                  [ Add rga.replicaId 3 [1, 2] data
                  , Add rga.replicaId 2 [1, 0] data
                  , Add rga.replicaId 1 [-1] data
                  , Add rga.replicaId -1 [0] Nothing
                  ]
          in
              expectRGAOperations operations result

        , test "sets last operation"
          <| always (expectLastOperation batch result)
        ]


describeBatchAtomicity description =
  test description <| \_ ->
    let
        rga =
          RGA.init { id = 0, maxReplicas = 1 }

        batch = Batch
          [ Add rga.replicaId 1 [0] data
          , Add rga.replicaId 2 [9] data
          ]

        result =
          applyLocal batch rga
    in
        Expect.err result


describeDelete description =
  let
      rga =
        RGA.init { id = 0, maxReplicas = 1 }

      batch = Batch
        [ Add rga.replicaId 1 [-1] data
        , Delete rga.replicaId [1]
        ]

      result =
        applyLocal batch rga
  in
      describe description
        [ test "apply Add delete succeeds"
          <| always (Expect.ok result)

        , test "apply Delete doesn't increment timestamp"
          <| always (expectRGATimestamp 1 result)

        , test "apply Delete result updates rga nodes" <| \_ ->
          expectRGANode [1] (tombstone 1 [1]) result

        , test "sets last operation"
          <| always (expectLastOperation batch result)
        ]


describeDeleteIsIdempotent description =
  let
      rga =
        RGA.init { id = 0, maxReplicas = 1 }

      batch = Batch
        [ Add rga.replicaId 1 [-1] data
        , Delete rga.replicaId [1]
        , Delete rga.replicaId [1]
        , Delete rga.replicaId [1]
        , Delete rga.replicaId [1]
        , Delete rga.replicaId [1]
        ]

      result =
        applyLocal batch rga
  in
      describe description

        [ test "apply Add multiple times succeeds"
          <| always (Expect.ok result)

        , test "apply Add multiple times result updates rga nodes" <| \_ ->
          expectRGANode [1] (tombstone 1 [1]) result

        , test "apply Add multiple times increments timestamp"
          <| always (expectRGATimestamp 1 result)

        , test "apply Add multiple times sets rga operations" <| \_ ->
          let
              operations =
                [ Delete rga.replicaId [1]
                , Add rga.replicaId 1 [-1] data
                , Add rga.replicaId -1 [0] Nothing
                ]
          in
              expectRGAOperations operations result

        , test "sets last operation" <| \_ ->
          let
              operation = Batch
                [ Add rga.replicaId 1 [-1] data
                , Delete rga.replicaId [1]
                ]
          in
              expectLastOperation operation result
        ]


describeOperationsSince description =
  let
      rga_ =
        RGA.init { id = 0, maxReplicas = 1 }

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
        applyLocal batch rga_
          |> Result.withDefault rga_
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
              Expect.equal operations <| operationsSince -1 rga

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
              Expect.equal operations <| operationsSince 2 rga

        , test "operations since last" <| \_ ->
          let
              operations =
                [ Add rga_.replicaId 6 [5] data ]
          in
              Expect.equal operations <| operationsSince 6 rga

        , test "not present returns empty" <| \_ ->
          Expect.equal [] <| operationsSince 10 rga
        ]


expectRGANode path exp result =
  expect (\rga ->
    Expect.equal (Just exp) (Node.descendant path rga.root)) result


expectRGATimestamp exp result =
  expect (\rga ->
    Expect.true
      ("expected RGA `" ++ Debug.toString rga ++
      "` to have timestamp " ++ Debug.toString exp)
    (exp == rga.timestamp)) result


expectRGAOperations exp result =
  expect (\rga -> Expect.equal exp rga.operations) result


expectLastOperation exp result =
  expect (\rga -> Expect.equal exp rga.lastOperation) result


expectRGAPointer exp result =
  expect (\rga -> Expect.equal exp rga.pointer) result


expect fun result =
  Result.map fun result
    |> Result.withDefault (Expect.fail "failed")


