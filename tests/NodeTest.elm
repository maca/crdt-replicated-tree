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
        [ describe "concurrent inserts order consistency"
            [ test "apply local first" <|
                \_ ->
                    localFirstExample
                        |> Node.map Node.value
                        |> Expect.equal
                            [ Just 'a'
                            , Just 'p'
                            , Just 'q'
                            , Just 'x'
                            , Just 'y'
                            , Just 'b'
                            , Just 'c'
                            ]

            --
            , test "apply remote first" <|
                \_ ->
                    remoteFirstExample
                        |> Node.map Node.value
                        |> Expect.equal
                            [ Just 'a'
                            , Just 'p'
                            , Just 'q'
                            , Just 'x'
                            , Just 'y'
                            , Just 'b'
                            , Just 'c'
                            ]
            ]

        --
        , test "map" <|
            \_ ->
                flatExample
                    |> Node.map Node.value
                    |> Expect.equal
                        [ Just 'a', Just 'b', Just 'c', Just 'd', Just 'e' ]
        , test "filter map" <|
            \_ ->
                flatExample
                    |> Node.filterMap Node.value
                    |> Expect.equal
                        [ 'a', 'b', 'c', 'd', 'e' ]

        --
        , test "find" <|
            \_ ->
                flatExample
                    |> Node.find (\n -> Node.value n == Just 'c')
                    |> Maybe.andThen Node.value
                    |> Expect.equal (Just 'c')

        --
        , test "descendant" <|
            \_ ->
                nestedExample
                    |> Node.descendant [ 1, 2, 3, 4 ]
                    |> Maybe.andThen Node.value
                    |> Expect.equal (Just 'd')

        --
        , test "path" <|
            \_ ->
                nestedExample
                    |> Node.descendant [ 1, 2, 3, 4 ]
                    |> Maybe.map Node.path
                    |> Expect.equal (Just [ 1, 2, 3, 4 ])

        --
        , test "timestamp" <|
            \_ ->
                nestedExample
                    |> Node.descendant [ 1, 2, 3, 4 ]
                    |> Maybe.map Node.timestamp
                    |> Expect.equal (Just 4)

        --
        , test "atIndex" <|
            \_ ->
                flatExample
                    |> Expect.all
                        [ Node.atIndex 0
                            >> Maybe.andThen Node.value
                            >> Expect.equal (Just 'a')
                        , Node.atIndex 1
                            >> Maybe.andThen Node.value
                            >> Expect.equal (Just 'b')
                        , Node.atIndex 2
                            >> Maybe.andThen Node.value
                            >> Expect.equal (Just 'c')
                        , Node.atIndex 3
                            >> Maybe.andThen Node.value
                            >> Expect.equal (Just 'd')
                        , Node.atIndex 4
                            >> Maybe.andThen Node.value
                            >> Expect.equal (Just 'e')
                        ]

        --
        , test "childIndex" <|
            \_ ->
                flatExample
                    |> Expect.all
                        [ Node.atIndex 0
                            >> Maybe.andThen
                                (\n -> Node.childIndex n flatExample)
                            >> Expect.equal (Just 0)
                        , Node.atIndex 1
                            >> Maybe.andThen
                                (\n -> Node.childIndex n flatExample)
                            >> Expect.equal (Just 1)
                        , Node.atIndex 2
                            >> Maybe.andThen
                                (\n -> Node.childIndex n flatExample)
                            >> Expect.equal (Just 2)
                        , Node.atIndex 3
                            >> Maybe.andThen
                                (\n -> Node.childIndex n flatExample)
                            >> Expect.equal (Just 3)
                        , Node.atIndex 4
                            >> Maybe.andThen
                                (\n -> Node.childIndex n flatExample)
                            >> Expect.equal (Just 4)
                        ]
        ]


commonExample =
    addAfter [ 0 ] ( 0x00100001, 'a' ) root
        |> Result.andThen (addAfter [ 0x00100001 ] ( 0x00200001, 'b' ))
        |> Result.andThen (addAfter [ 0x00200001 ] ( 0x00300001, 'c' ))
        |> Result.withDefault root


localFirstExample =
    addAfter [ 0x00100001 ] ( 0x00400001, 'x' ) commonExample
        |> Result.andThen (addAfter [ 0x00400001 ] ( 0x00500001, 'y' ))
        |> Result.andThen (addAfter [ 0x00100001 ] ( 0x00400002, 'p' ))
        |> Result.andThen (addAfter [ 0x00400002 ] ( 0x00500002, 'q' ))
        |> Result.withDefault root


remoteFirstExample =
    addAfter [ 0x00100001 ] ( 0x00400002, 'p' ) commonExample
        |> Result.andThen (addAfter [ 0x00400002 ] ( 0x00500002, 'q' ))
        |> Result.andThen (addAfter [ 0x00100001 ] ( 0x00400001, 'x' ))
        |> Result.andThen (addAfter [ 0x00400001 ] ( 0x00500001, 'y' ))
        |> Result.withDefault root


flatExample =
    addAfter [ 0 ] ( 1, 'a' ) root
        |> Result.andThen (addAfter [ 1 ] ( 2, 'b' ))
        |> Result.andThen (addAfter [ 2 ] ( 3, 'c' ))
        |> Result.andThen (addAfter [ 3 ] ( 4, 'd' ))
        |> Result.andThen (addAfter [ 1 ] ( 5, 'z' ))
        |> Result.andThen (Node.delete [ 5 ])
        |> Result.andThen (addAfter [ 4 ] ( 6, 'e' ))
        |> Result.withDefault root


nestedExample =
    addAfter [ 0 ] ( 1, 'a' ) root
        |> Result.andThen (addAfter [ 1, 0 ] ( 2, 'b' ))
        |> Result.andThen (addAfter [ 1, 2, 0 ] ( 3, 'c' ))
        |> Result.andThen (addAfter [ 1, 2, 3, 0 ] ( 4, 'd' ))
        |> Result.withDefault root
