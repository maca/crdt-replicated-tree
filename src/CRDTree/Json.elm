module CRDTree.Json exposing (operationEncoder, operationDecoder)

{-| Replicated Tree JSON serialization

@docs operationEncoder
@docs operationDecoder
-}


import Json.Decode as Decode exposing
  ( Decoder
  , field
  , succeed
  , decodeValue
  )
import Json.Encode as Encode exposing
  ( Value
  , int
  , string
  , list
  )
import CRDTree.Operation exposing (Operation(..))
import CRDTree.ReplicaId as ReplicaId exposing (ReplicaId)

import Dict exposing (Dict)


{-| Encoder for an operation, expects a function that takes
anything and returns a value
-}
operationEncoder : (a -> Value) -> Operation a -> Value
operationEncoder valueEncoder operation =
  case operation of
    Add replicaId ts path value ->
      Encode.object
        [ ( "op", string "add" )
        , ( "path", (list int path) )
        , ( "rid", int <| ReplicaId.toInt replicaId )
        , ( "ts", int ts )
        , ( "val", valueEncoder value )
        ]

    Delete replicaId path ->
      Encode.object
        [ ( "op", string "del" )
        , ( "rid", int <| ReplicaId.toInt replicaId )
        , ( "path", (list int path) )
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
  (field "op" Decode.string)
    |> Decode.andThen (operationDecoderHelp valueDecoder)


operationDecoderHelp : Decoder a -> String -> Decoder (Operation a)
operationDecoderHelp valueDecoder operationType =
  case operationType of
    "add" ->
      Decode.map4 Add
        (field "rid" (Decode.int |> Decode.map ReplicaId.fromInt))
        (field "ts" Decode.int)
        (field "path" <| Decode.list Decode.int)
        (field "val" valueDecoder)

    "del" ->
      Decode.map2 Delete
        (field "rid" (Decode.int |> Decode.map ReplicaId.fromInt))
        (field "path" <| Decode.list Decode.int)

    "batch" ->
      Decode.map Batch
        (field "ops" <| Decode.list (operationDecoder valueDecoder))

    _ ->
      Decode.succeed <| Batch []

