module JsonTest exposing (..)

import Bitwise
import Dict exposing (Dict, fromList, empty)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import Tuple

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Test exposing (..)
import List exposing (map, reverse)

import RG exposing (Operation(..), ReplicaId(..))

import RG.Json exposing
  ( operationEncoder
  , operationDecoder
  )

type Data = Data

data = Just Data


suite : Test
suite = describe "encode/decode"
  [ describe "operations"
    [ test "Add" <| \_ ->
      let
          operation =
            Add (ReplicaId 1) 3 [1, 2] data

          value =
            operationEncoder dataEncoder operation

          result =
            decodeValue (operationDecoder dataDecoder) value
      in
          expectResult operation result

    , test "Delete" <| \_ ->
      let
          operation =
            Delete (ReplicaId 1) [1, 2]

          value =
            operationEncoder dataEncoder operation

          result =
            decodeValue (operationDecoder dataDecoder) value
      in
          expectResult operation result

    , test "Batch" <| \_ ->
      let
          operation =
            Batch
              [ Add (ReplicaId 1) 3 [1, 2] data
              , Add (ReplicaId 1) 4 [1, 3] Nothing
              , Delete (ReplicaId 1) [1, 2]
              ]

          value =
            operationEncoder dataEncoder operation

          result =
            decodeValue (operationDecoder dataDecoder) value
      in
          expectResult operation result
    ]
  ]



dataEncoder: Data -> Encode.Value
dataEncoder _ =
  Encode.string "data"


dataDecoder: Decode.Decoder Data
dataDecoder =
  Decode.succeed Data



expectResult expected result =
  mapResult (Expect.equal expected) result


mapResult fun result =
  Result.map fun result
    |> Result.withDefault (Expect.fail "failed")


