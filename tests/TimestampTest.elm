module TimestampTest exposing (..)

import CRDTree.Operation as Operation exposing (Operation(..))
import CRDTree.Timestamp
    exposing
        ( increment
        , incrementTo
        , init
        , operationId
        , replicaId
        )
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Result
import Test exposing (..)


replicaBits =
    20


maxReplicas =
    (2 ^ replicaBits) - 2


fuzzer =
    Fuzz.tuple ( intRange 1 maxReplicas, intRange 1 10000 )


bigFuzzer =
    Fuzz.tuple
        ( intRange 1 maxReplicas
        , intRange 1 ((2 ^ 31) - 2)
        )


suite : Test
suite =
    describe "Timestamp"
        [ test "increment" <|
            \_ ->
                init 5
                    |> Expect.all
                        [ Expect.equal 0x05
                        , increment >> Expect.equal 0x00100005
                        , increment
                            >> increment
                            >> Expect.equal 0x00200005
                        ]

        --
        , fuzz fuzzer "replica id" <|
            \( id, _ ) ->
                init id
                    |> Expect.all
                        [ replicaId >> Expect.equal id
                        , increment >> replicaId >> Expect.equal id
                        ]

        --
        , fuzz fuzzer "increment fuzz" <|
            \( id, count ) ->
                List.range 1 count
                    |> List.foldl (always increment) (init id)
                    |> Expect.all
                        [ operationId >> Expect.equal count
                        , replicaId >> Expect.equal id
                        ]

        --
        , test "increment to greater than" <|
            \_ ->
                init 5
                    |> Expect.all
                        [ Expect.equal 5
                        , incrementTo 0x00500001
                            >> Expect.equal 0x00500005
                        , incrementTo 0x00500005
                            >> Expect.equal 0x00500005
                        , incrementTo 0x00500006
                            >> Expect.equal 0x00500005
                        , incrementTo 0x00500001
                            >> replicaId
                            >> Expect.equal 5
                        ]

        --
        , fuzz bigFuzzer "increment to greater than fuzz" <|
            \( id, count ) ->
                let
                    ref =
                        count * maxReplicas + id
                in
                init (id + 1)
                    |> incrementTo ref
                    |> Expect.all
                        [ operationId
                            >> Expect.equal (operationId ref)
                        , replicaId >> Expect.equal (id + 1)
                        ]
        ]
