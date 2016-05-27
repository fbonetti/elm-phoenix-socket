module Phoenix.Helpers exposing (..)

import Json.Decode as JD exposing ((:=))
import Json.Encode as JE

type alias Message =
  { topic : String
  , event : String
  , payload : JD.Value
  , ref : Maybe Int
  }

nullOrInt : JD.Decoder (Maybe Int)
nullOrInt =
  JD.oneOf
    [ JD.null Nothing
    , JD.map Just JD.int
    ]


maybeInt : Maybe Int -> JE.Value
maybeInt maybe =
  case maybe of
    Just num ->
      JE.int num

    Nothing ->
      JE.null


messageDecoder : JD.Decoder Message
messageDecoder =
  JD.object4 Message
    ("topic" := JD.string)
    ("event" := JD.string)
    ("payload" := JD.value)
    ("ref" := nullOrInt)


messageEncoder : Message -> JE.Value
messageEncoder { topic, event, payload, ref } =
  JE.object
    [ ( "topic", JE.string topic )
    , ( "event", JE.string event )
    , ( "payload", payload )
    , ( "ref", maybeInt ref )
    ]


encodeMessage : Message -> String
encodeMessage =
  messageEncoder >> JE.encode 0


emptyPayload : JE.Value
emptyPayload =
  JE.object []
