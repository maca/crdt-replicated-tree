module ListTest exposing (..)

import Result exposing (Result(..))

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Test exposing (..)
import RGA.List exposing (find, insertWhen, replaceWhen)

import RGA.List as NodeList

suite : Test
suite = describe "RGA List"
  [ describe "insertWhen"
    [ test "inserts when predicate is satisfied" <| \_ ->
      let
          result = [1, 2, 3, 4, 5]
            |> insertWhen ((==) 4) 6
      in
          expectResult [1, 2, 3, 6, 4, 5] result

    , test "inserts at first position" <| \_ ->
      let
          result = [1, 2, 3, 4, 5]
            |> insertWhen ((==) 1) 6
      in
          expectResult [6, 1, 2, 3, 4, 5] result

    , test "inserts before last position" <| \_ ->
      let
          result = [1, 2, 3, 4, 5]
            |> insertWhen ((==) 5) 6
      in
          expectResult [1, 2, 3, 4, 6, 5] result


    -- , test "returns same if no match" <| \_ ->
    --   let
    --       result = [1, 2] |> insertWhen ((==) 4) 6
    --   in
    --       expectResult [1, 2] result
    ]

  , describe "replaceWhen"
    [ test "inserts when predicate is satisfied" <| \_ ->
      let
          result = [1, 2, 3, 4, 5] |> replaceWhen ((==) 4) 6
      in
          expectResult [1, 2, 3, 6, 5] result
    ]
  ]


expectResult expected result =
  mapResult (Expect.equal expected) result


mapResult fun result =
  Result.map fun result
    |> Result.withDefault (Expect.fail "failed")


