module RGA.Json exposing (operationEncoder, operationDecoder)

{-| RGA JSON serialization

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
import RGA exposing (Operation(..), ReplicaId(..))


{-| Encoder for an operation, expects a function that takes
anything and returns a value
-}
operationEncoder : (a -> Value) -> Operation a -> Value
operationEncoder encoder operation =
  case operation of
    Add (ReplicaId id) ts path a ->
      Encode.object
        [ ( "op", string "add" )
        , ( "path", (list int path) )
        , ( "rid", int id )
        , ( "ts", int ts )
        , ( "a", Maybe.map encoder a
                  |> Maybe.withDefault Encode.null
          )
        ]

    Delete (ReplicaId id) path ->
      Encode.object
        [ ( "op", string "del" )
        , ( "rid", int id )
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
        (field "rid" (Decode.int |> Decode.map ReplicaId))
        (field "ts" Decode.int)
        (field "path" <| Decode.list Decode.int)
        (field "a" (oneOf [null Nothing, maybe decoder]))

    "del" ->
      Decode.map2 Delete
        (field "rid" (Decode.int |> Decode.map ReplicaId))
        (field "path" <| Decode.list Decode.int)

    "batch" ->
      Decode.map Batch
        (field "ops" <| Decode.list (operationDecoder decoder))

    _ ->
      Decode.succeed <| Batch []


