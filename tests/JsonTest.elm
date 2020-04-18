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

import CRDTree.Operation exposing (Operation(..))


import CRDTree.Json exposing
  ( operationEncoder
  , operationDecoder
  )


suite : Test
suite = describe "encode/decode"
  [ describe "operations"
    [ test "Add" <| \_ ->
      let
          operation = Add 3 [1, 2] "a"

          value =
            operationEncoder Encode.string operation

          result =
            decodeValue (operationDecoder Decode.string) value
      in
          expectResult operation result

    , test "Delete" <| \_ ->
      let
          operation = Delete [1, 2]

          value =
            operationEncoder Encode.string operation

          result =
            decodeValue (operationDecoder Decode.string) value
      in
          expectResult operation result

    , test "Batch" <| \_ ->
      let
          operation =
            Batch
              [ Add 3 [1, 2] "a"
              , Add 4 [1, 3] "b"
              , Delete [1, 2]
              ]

          value =
            operationEncoder Encode.string operation

          result =
            decodeValue (operationDecoder Decode.string) value
      in
          expectResult operation result
    ]
  ]


expectResult expected result =
  mapResult (Expect.equal expected) result


mapResult fun result =
  Result.map fun result
    |> Result.withDefault (Expect.fail "failed")
