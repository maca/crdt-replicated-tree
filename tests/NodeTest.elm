module NodeTest exposing (..)

import CRDTree.Node as Node
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Internal.Node
    exposing
        ( Error(..)
        , Node
        , addAfter
        , children
        , delete
        , root
        )
import Internal.Operation as Operation exposing (Operation(..))
import Result
import Test exposing (..)


suite : Test
suite =
    describe "Node"
        [ describe "add order"
            [ test "append bigger first" <|
                \_ ->
                    appendSmallerFirstExample
                        |> Node.map Node.value
                        |> Expect.equal
                            [ Just 'b', Just 'a' ]
            , test "append smaller first" <|
                \_ ->
                    appendBiggerFirstExample
                        |> Node.map Node.value
                        |> Expect.equal
                            [ Just 'b', Just 'a' ]
            , test "insert smaller first" <|
                \_ ->
                    insertSmallerFirstExample
                        |> Node.map Node.value
                        |> Expect.equal
                            [ Just 1
                            , Just 6
                            , Just 5
                            , Just 4
                            , Just 2
                            , Just 3
                            ]
            , test "insert bigger first" <|
                \_ ->
                    insertBiggerFirstExample
                        |> Node.map Node.value
                        |> Expect.equal
                            [ Just 1
                            , Just 6
                            , Just 5
                            , Just 4
                            , Just 2
                            , Just 3
                            ]
            ]
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
        , test "map" <|
            \_ ->
                flatExample
                    |> Node.map Node.value
                    |> Expect.equal
                        [ Just 'a', Just 'b', Just 'c', Just 'd' ]
        , test "filterMap" <|
            \_ ->
                flatExample
                    |> Node.filterMap Node.value
                    |> Expect.equal
                        [ 'a', 'b', 'c', 'd' ]
        , test "foldl" <|
            \_ ->
                flatExample
                    |> Node.foldl (\n acc -> acc ++ [ Node.value n ]) []
                    |> Expect.equal
                        [ Just 'a', Just 'b', Just 'c', Just 'd' ]
        , test "foldr" <|
            \_ ->
                flatExample
                    |> Node.foldr (\n acc -> Node.value n :: acc) []
                    |> Expect.equal
                        [ Just 'a', Just 'b', Just 'c', Just 'd' ]
        , test "loop" <|
            \_ ->
                flatExample
                    |> Node.loop
                        (\n acc ->
                            if Node.value n == Just 'c' then
                                Node.Done acc

                            else
                                Node.Take (acc ++ [ Node.value n ])
                        )
                        []
                    |> Expect.equal
                        [ Just 'a', Just 'b' ]
        , test "head" <|
            \_ ->
                flatExample
                    |> Node.head
                    |> Maybe.andThen Node.value
                    |> Expect.equal (Just 'a')
        , test "last" <|
            \_ ->
                flatExample
                    |> Node.last
                    |> Maybe.andThen Node.value
                    |> Expect.equal (Just 'd')
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
        |> Result.andThen (addAfter [ 1 ] ( 4, 4 ))
        |> Result.andThen (addAfter [ 1 ] ( 6, 6 ))
        |> Result.andThen (addAfter [ 1 ] ( 5, 5 ))
        |> Result.withDefault root


flatExample =
    addAfter [ 0 ] ( 1, 'a' ) root
        |> Result.andThen (addAfter [ 1 ] ( 2, 'b' ))
        |> Result.andThen (addAfter [ 2 ] ( 3, 'x' ))
        |> Result.andThen (addAfter [ 3 ] ( 4, 'c' ))
        |> Result.andThen (addAfter [ 4 ] ( 5, 'd' ))
        |> Result.andThen (delete [ 3 ])
        |> Result.withDefault root


nestedExample =
    addAfter [ 0 ] ( 1, 'a' ) root
        |> Result.andThen (addAfter [ 1, 0 ] ( 2, 'b' ))
        |> Result.andThen (addAfter [ 1, 2, 0 ] ( 3, 'c' ))
        |> Result.andThen (addAfter [ 1, 2, 3, 0 ] ( 4, 'd' ))
        |> Result.withDefault root
