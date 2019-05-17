module RG.Json exposing (operationEncoder, operationDecoder)

{-| Replicated Graph JSON serialization

@docs operationEncoder
@docs operationDecoder
-}


import Json.Decode as Decode exposing
  ( Decoder
  , field
  , succeed
  , decodeValue
  , oneOf
  , maybe
  , null
  )
import Json.Encode as Encode exposing
  ( Value
  , int
  , string
  , list
  )
import RG.Operation exposing (Operation(..))
import RG.ReplicaId as ReplicaId exposing (ReplicaId)

import Dict exposing (Dict)


{-| Encoder for an operation, expects a function that takes
anything and returns a value
-}
operationEncoder : (a -> Value) -> Operation a -> Value
operationEncoder encoder operation =
  case operation of
    Add replicaId ts path a ->
      Encode.object
        [ ( "op", string "add" )
        , ( "path", (list int path) )
        , ( "rid", int <| ReplicaId.toInt replicaId )
        , ( "ts", int ts )
        , ( "a", Maybe.map encoder a
                  |> Maybe.withDefault Encode.null
          )
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
        , ( "ops", list (operationEncoder encoder) operations )
        ]


{-| Decoder for an operation
-}
operationDecoder : Decoder a -> Decoder (Operation a)
operationDecoder decoder =
  (field "op" Decode.string)
    |> Decode.andThen (operationDecoderHelp decoder)


operationDecoderHelp : Decoder a -> String -> Decoder (Operation a)
operationDecoderHelp decoder operationType =
  case operationType of
    "add" ->
      Decode.map4 Add
        (field "rid" (Decode.int |> Decode.map ReplicaId.fromInt))
        (field "ts" Decode.int)
        (field "path" <| Decode.list Decode.int)
        (field "a" (oneOf [null Nothing, maybe decoder]))

    "del" ->
      Decode.map2 Delete
        (field "rid" (Decode.int |> Decode.map ReplicaId.fromInt))
        (field "path" <| Decode.list Decode.int)

    "batch" ->
      Decode.map Batch
        (field "ops" <| Decode.list (operationDecoder decoder))

    _ ->
      Decode.succeed <| Batch []




{-| Last known timestamp for a given replica
not used yet
-}
lastReplicaTimestamp : Int -> Dict Int Int -> Int
lastReplicaTimestamp rid replicas =
  Dict.get rid replicas |> Maybe.withDefault -1


