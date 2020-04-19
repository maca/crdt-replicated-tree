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
                exampleOne
                    |> Node.map (\t n -> ( t, Node.value n ))
                    |> Expect.equal
                        [ ( 1, Just 'a' )
                        , ( 2, Just 'b' )
                        , ( 3, Just 'c' )
                        , ( 4, Just 'd' )
                        ]
        , test "filter map" <|
            \_ ->
                exampleOne
                    |> Node.filterMap (\_ n -> Node.value n)
                    |> Expect.equal
                        [ 'a', 'b', 'c', 'd' ]
        ]


exampleOne =
    addAfter 0 ( 1, 'a' ) root
        |> Result.andThen (addAfter 1 ( 2, 'b' ))
        |> Result.andThen (addAfter 2 ( 3, 'c' ))
        |> Result.andThen (addAfter 3 ( 4, 'd' ))
        |> Result.withDefault root
