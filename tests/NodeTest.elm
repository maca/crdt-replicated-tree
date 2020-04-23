module NodeTest exposing (..)

import CRDTree.Node as Node
    exposing
        ( Error(..)
        , Node
        , addAfter
        , children
        , root
        )
import CRDTree.Operation as Operation exposing (Operation(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Result
import Test exposing (..)


suite : Test
suite =
    describe "Node"
        [ test "map" <|
            \_ ->
                flatExample
                    |> Node.map Node.value
                    |> Expect.equal
                        [ Just 'a', Just 'b', Just 'c', Just 'd' ]
        , test "filter map" <|
            \_ ->
                flatExample
                    |> Node.filterMap Node.value
                    |> Expect.equal
                        [ 'a', 'b', 'c', 'd' ]
        , test "find" <|
            \_ ->
                flatExample
                    |> Node.find (\n -> Node.value n == Just 'c')
                    |> Maybe.andThen Node.value
                    |> Expect.equal (Just 'c')
        , test "descendant" <|
            \_ ->
                nestedExample
                    |> Node.descendant [ 1, 2, 3, 4 ]
                    |> Maybe.andThen Node.value
                    |> Expect.equal (Just 'd')
        , test "path" <|
            \_ ->
                nestedExample
                    |> Node.descendant [ 1, 2, 3, 4 ]
                    |> Maybe.map Node.path
                    |> Expect.equal (Just [ 1, 2, 3, 4 ])
        , test "timestamp" <|
            \_ ->
                nestedExample
                    |> Node.descendant [ 1, 2, 3, 4 ]
                    |> Maybe.map Node.timestamp
                    |> Expect.equal (Just 4)
        ]


flatExample =
    addAfter [ 0 ] ( 1, 'a' ) root
        |> Result.andThen (addAfter [ 1 ] ( 2, 'b' ))
        |> Result.andThen (addAfter [ 2 ] ( 3, 'c' ))
        |> Result.andThen (addAfter [ 3 ] ( 4, 'd' ))
        |> Result.withDefault root


nestedExample =
    addAfter [ 0 ] ( 1, 'a' ) root
        |> Result.andThen (addAfter [ 1, 0 ] ( 2, 'b' ))
        |> Result.andThen (addAfter [ 1, 2, 0 ] ( 3, 'c' ))
        |> Result.andThen (addAfter [ 1, 2, 3, 0 ] ( 4, 'd' ))
        |> Result.withDefault root
