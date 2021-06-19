module NodeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Internal.Node as Node
    exposing
        ( Error(..)
        , Node
        , addAfter
        , children
        , root
        )
import Internal.Operation as Operation exposing (Operation(..))
import Result
import Test exposing (..)


suite : Test
suite =
    describe "Node"
        [ describe "add order"
            [ test "append smaller first" <|
                \_ ->
                    appendSmallerFirstExample
                        |> Node.map Node.value
                        |> Expect.equal
                            [ Just 'a', Just 'b' ]
            , test "append bigger first" <|
                \_ ->
                    appendBiggerFirstExample
                        |> Node.map Node.value
                        |> Expect.equal
                            [ Just 'a', Just 'b' ]
            , test "insert smaller first" <|
                \_ ->
                    insertSmallerFirstExample
                        |> Node.map Node.value
                        |> Expect.equal
                            [ Just 1
                            , Just 2
                            , Just 3
                            , Just 4
                            , Just 5
                            , Just 6
                            ]
            , test "insert bigger first" <|
                \_ ->
                    insertBiggerFirstExample
                        |> Node.map Node.value
                        |> Expect.equal
                            [ Just 1
                            , Just 2
                            , Just 3
                            , Just 4
                            , Just 5
                            , Just 6
                            ]
            ]
        , test "map" <|
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


appendSmallerFirstExample =
    addAfter [ 0 ] ( 1, 'a' ) root
        |> Result.andThen (addAfter [ 0 ] ( 2, 'b' ))
        |> Result.withDefault root


appendBiggerFirstExample =
    addAfter [ 0 ] ( 2, 'b' ) root
        |> Result.andThen (addAfter [ 0 ] ( 1, 'a' ))
        |> Result.withDefault root


insertSmallerFirstExample =
    addAfter [ 0 ] ( 1, 1 ) root
        |> Result.andThen (addAfter [ 1 ] ( 2, 2 ))
        |> Result.andThen (addAfter [ 2 ] ( 3, 3 ))
        |> Result.andThen (addAfter [ 1 ] ( 6, 6 ))
        |> Result.andThen (addAfter [ 1 ] ( 5, 5 ))
        |> Result.andThen (addAfter [ 1 ] ( 4, 4 ))
        |> Result.withDefault root


insertBiggerFirstExample =
    addAfter [ 0 ] ( 1, 1 ) root
        |> Result.andThen (addAfter [ 1 ] ( 2, 2 ))
        |> Result.andThen (addAfter [ 2 ] ( 3, 3 ))
        |> Result.andThen (addAfter [ 1 ] ( 6, 6 ))
        |> Result.andThen (addAfter [ 1 ] ( 4, 4 ))
        |> Result.andThen (addAfter [ 1 ] ( 5, 5 ))
        |> Result.withDefault root


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
