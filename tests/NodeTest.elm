module NodeTest exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Set

import Shrink exposing (noShrink)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, char, list)
import Test exposing (..)


import CRDTree.Node exposing
  ( root
  , init
  , tombstone
  , addAfter
  , delete
  , children
  , timestamp
  , Error(..)
  )


suite : Test
suite = describe "Node"
  [ describe "addAfter"
    [ test "addAfter beginning of an empty list" <| \_ ->
      addAfter 0 (1, 'a') root
        |> expectSuccessWithChildren [ init 'a' [1] ]

    , test "addAfter timestamp appends node" <| \_ ->
      addAfter 0 (1, 'a') root
        |> Result.andThen (addAfter 1 (2, 'b'))
        |> expectSuccessWithChildren
          [ init 'b' [2], init 'a' [1] ]

    , test "addAfter timestamp appends consecutive nodes" <| \_ ->
      addAfter 0 (1, 'a') root
        |> Result.andThen (addAfter 1 (2, 'b'))
        |> Result.andThen (addAfter 2 (3, 'c'))
        |> expectSuccessWithChildren
          [ init 'c' [3], init 'b' [2], init 'a' [1] ]

    , test "addAfter same timestamp" <|
        \_ ->
          addAfter 0 (1, 1) root
            |> Result.andThen (addAfter 1 (5, 5))
            |> Result.andThen (addAfter 1 (3, 3))
            |> Result.andThen (addAfter 1 (2, 2))
            |> Result.andThen (addAfter 1 (4, 4))
            |> expectSuccessWithChildren
              [ init 2 [2]
              , init 3 [3]
              , init 4 [4]
              , init 5 [5]
              , init 1 [1]
              ]

    , fuzz (list <| Fuzz.intRange 10 100)
      "addAfter has deterministic order" <|
        \list ->
          let
              dedup =
                Set.toList <| Set.fromList list

              initial =
                addAfter 0 (1, 1) root
                  |> Result.andThen (addAfter 1 (2, 2))
                  |> Result.andThen (addAfter 2 (3, 3))

              addF prev i result =
                Result.andThen (addAfter prev (i, i)) result

              expected =
                [ init 3 [3], init 2 [2] ] ++
                (dedup
                  |> List.sort
                  |> List.map (\i -> init i [i])) ++
                [ init 1 [1], init 101 [101] ]
          in
          dedup
            |> List.foldl (addF 1) initial
            |> Result.andThen (addAfter 0 (101, 101))
            |> expectSuccessWithChildren expected

    , test "addAfter non existant node fails" <| \_ ->
      Expect.equal
        (addAfter 10 (1, 'a') root)
        (Err NotFound)

    , test "addAfter negative timestamp fails" <| \_ ->
      Expect.equal
        (addAfter -10 (1, 'a') root)
        (Err BadTimestamp)

    , test "addAfter twice fails" <| \_ ->
      addAfter 0 (1, 'a') root
        |> Result.andThen (addAfter 0 (1, 'a'))
        |> Expect.equal (Err AlreadyApplied)
    ]

  , describe "delete"
    [ test "delete marks node as tombstone" <| \_ ->
      addAfter 0 (1, 'a') root
        |> Result.andThen (delete 1)
        |> expectSuccessWithChildren [ tombstone [ 1 ] ]

    , test "delete non existing fails" <| \_ ->
      addAfter 0 (1, 'a') root
        |> Result.andThen (delete 10)
        |> Expect.equal (Err NotFound)

    , test "delete with negative timestamp fails" <| \_ ->
      addAfter 0 (1, 'a') root
        |> Result.andThen (delete -10)
        |> Expect.equal (Err BadTimestamp)

    , test "delete with timestamp 0 fails" <| \_ ->
      addAfter 0 (1, 'a') root
        |> Result.andThen (delete 0)
        |> Expect.equal (Err BadTimestamp)

    , test "delete twice fails" <| \_ ->
      addAfter 0 (1, 'a') root
        |> Result.andThen (delete 1)
        |> Result.andThen (delete 1)
        |> Expect.equal (Err AlreadyApplied)
    ]
  ]


expectSuccessWithChildren ns result =
  result
    |> Expect.all
      [ Expect.ok
      , Result.map (\n -> Expect.equal ns (children n) )
        >> Result.withDefault Expect.pass
      ]


