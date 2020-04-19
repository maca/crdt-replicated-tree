module CRDTree.Json exposing
    ( operationEncoder
    , operationDecoder
    )

{-| Replicated Tree JSON serialization

@docs operationEncoder
@docs operationDecoder

-}

import CRDTree.Operation exposing (Operation(..))
import Json.Decode as Decode
    exposing
        ( Decoder
        , field
        , succeed
        )
import Json.Encode as Encode
    exposing
        ( Value
        , int
        , list
        , string
        )


{-| Encoder for an operation, expects a function that takes
anything and returns a value
-}
operationEncoder : (a -> Value) -> Operation a -> Value
operationEncoder valueEncoder operation =
    case operation of
        Add ts path value ->
            Encode.object
                [ ( "op", string "add" )
                , ( "path", list int path )
                , ( "ts", int ts )
                , ( "val", valueEncoder value )
                ]

        Delete path ->
            Encode.object
                [ ( "op", string "del" )
                , ( "path", list int path )
                ]

        Batch operations ->
            Encode.object
                [ ( "op", string "batch" )
                , ( "ops", list (operationEncoder valueEncoder) operations )
                ]


{-| Decoder for an operation
-}
operationDecoder : Decoder a -> Decoder (Operation a)
operationDecoder valueDecoder =
    field "op" Decode.string
        |> Decode.andThen (operationDecoderHelp valueDecoder)


operationDecoderHelp : Decoder a -> String -> Decoder (Operation a)
operationDecoderHelp valueDecoder operationType =
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
                (field "ops" <| Decode.list (operationDecoder valueDecoder))

        _ ->
            Decode.succeed <| Batch []
