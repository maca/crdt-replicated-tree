module CRDTree.Operation exposing
    ( Operation
    , timestamp
    , path
    , replicaId
    , since
    , toList
    , fromList
    , merge
    , encoder
    , decoder
    )

{-| Represents an CRDTree operation

@docs Operation


# Properties

@docs timestamp
@docs path
@docs replicaId


# List

@docs since
@docs toList
@docs fromList


# Manipulation

@docs merge


# JSON

@docs encoder
@docs decoder

-}

import CRDTree.Timestamp as Timestamp
import Internal.Operation as Operation exposing (Operation(..))
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode exposing (Value, int, list, string)


{-| Represents an CRDTree operation
-}
type alias Operation a =
    Operation.Operation a


{-| Return operations since a timestamp
-}
since : Int -> List (Operation a) -> List (Operation a)
since =
    Operation.since


{-| Operation to List of Operations
-}
toList : Operation a -> List (Operation a)
toList =
    Operation.toList


{-| List of Operations to Batch
-}
fromList : List (Operation a) -> Operation a
fromList =
    Operation.fromList


{-| Merge two operations
-}
merge : Operation a -> Operation a -> Operation a
merge =
    Operation.merge


{-| Id of the replica originating the operation
-}
replicaId : Operation a -> Maybe Int
replicaId =
    Operation.replicaId


{-| Timestamp of the operation
-}
timestamp : Operation a -> Maybe Int
timestamp =
    Operation.timestamp


{-| Operation path
-}
path : Operation a -> Maybe (List Int)
path =
    Operation.path


{-| Encoder for an operation, expects a function that takes
anything and returns a value
-}
encoder : (a -> Value) -> Operation a -> Value
encoder valueEncoder operation =
    case operation of
        Add ts nodePath value ->
            Encode.object
                [ ( "op", string "add" )
                , ( "path", list int nodePath )
                , ( "ts", int ts )
                , ( "val", valueEncoder value )
                ]

        Delete nodePath ->
            Encode.object
                [ ( "op", string "del" )
                , ( "path", list int nodePath )
                ]

        Batch operations ->
            Encode.object
                [ ( "op", string "batch" )
                , ( "ops", list (encoder valueEncoder) operations )
                ]


{-| Decoder for an operation
-}
decoder : Decoder a -> Decoder (Operation a)
decoder valueDecoder =
    field "op" Decode.string
        |> Decode.andThen (decoderHelp valueDecoder)


decoderHelp : Decoder a -> String -> Decoder (Operation a)
decoderHelp valueDecoder operationType =
    case operationType of
        "add" ->
            Decode.map3 Add
                (field "ts" Decode.int)
                (field "path" <| Decode.list Decode.int)
                (field "val" valueDecoder)

        "del" ->
            Decode.map Delete
                (field "path" <| Decode.list Decode.int)

        "batch" ->
            Decode.map Batch
                (field "ops" <| Decode.list (decoder valueDecoder))

        _ ->
            Decode.succeed <| Batch []
