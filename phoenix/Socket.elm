module Phoenix.Socket exposing (Socket, Msg, init, join)

import Dict exposing (Dict)
import WebSocket
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))

-- MODEL

init : String -> (Msg -> msg) -> Socket msg
init path wrapper =
  Socket path wrapper (Dict.fromList [])

initChannel : Channel
initChannel =
  Channel (Dict.fromList []) ChannelJoining

type alias Socket msg =
  { path : String
  , wrapper : (Msg -> msg)
  , channels : Dict String Channel
  }

type alias Channel =
  { events : Dict String Event
  , state : ChannelState
  }

type ChannelState
    = ChannelClosed
    | ChannelErrored
    | ChannelJoined
    | ChannelJoining
    | ChannelLeaving

type alias Event = String

type Msg
    = NoOp

-- HELPERS

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
messageEncoder {topic,event,payload,ref} =
  JE.object
    [ ( "topic", JE.string topic )
    , ( "event", JE.string event )
    , ( "payload", payload )
    , ( "ref", maybeInt ref )
    ]

encodeMessage : Message -> String
encodeMessage =
  messageEncoder >> JE.encode 0

-- SUBSCRIPTIONS

listen : String -> (String -> msg) -> Sub msg
listen =
  WebSocket.listen


-- COMMANDS

join : String -> JE.Value -> Socket msg -> (Socket msg, Cmd msg)
join channel params socket =
  if Dict.member channel socket.channels then
    ( socket, Cmd.none )
  else
    ( { socket | channels = Dict.insert channel initChannel socket.channels }
    , WebSocket.send socket.path (joinChannelMessage channel params)
    )

joinChannelMessage : String -> JE.Value -> String
joinChannelMessage channel params =
  encodeMessage (Message channel "phx_join" params Nothing)

-- UPDATE

update : Msg -> Socket msg -> Socket msg
update msg socket =
  socket
