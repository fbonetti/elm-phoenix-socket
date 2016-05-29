module Phoenix.Helpers exposing (..)

import Json.Decode as JD exposing ((:=))
import Json.Encode as JE


type alias Message =
  { event : String
  , topic : String
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
    ("event" := JD.string)
    ("topic" := JD.string)
    ("payload" := JD.value)
    ("ref" := nullOrInt)


messageEncoder : Message -> JE.Value
messageEncoder { topic, event, payload, ref } =
  JE.object
    [ ( "event", JE.string event )
    , ( "topic", JE.string topic )
    , ( "payload", payload )
    , ( "ref", maybeInt ref )
    ]


encodeMessage : Message -> String
encodeMessage =
  messageEncoder >> JE.encode 0


emptyPayload : JE.Value
emptyPayload =
  JE.object []
