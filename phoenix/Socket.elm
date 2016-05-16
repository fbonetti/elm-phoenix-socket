module Phoenix.Socket exposing (Socket, Msg, init, join, update, on, listen)

import Dict exposing (Dict)
import WebSocket
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))

-- MODEL

init : String -> (Msg -> msg) -> Socket msg
init path wrapper =
  Socket path wrapper (Dict.fromList []) (Dict.fromList [])

type alias Socket msg =
  { path : String
  , wrapper : (Msg -> msg)
  , channels : Dict String ChannelState
  , events : Dict (Event, Channel) (JD.Decoder msg)
  }

type ChannelState
    = ChannelClosed
    | ChannelErrored
    | ChannelJoined
    | ChannelJoining
    | ChannelLeaving

type alias Channel = String
type alias Event = String

type Msg
    = NoOp
    | SetChannelState String ChannelState

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

listen : Socket msg -> Sub msg
listen socket =
  WebSocket.listen socket.path (handleMessage socket)

handleMessage : Socket msg -> String -> msg
handleMessage socket strMessage =
  let a = Debug.log "rawMessage" strMessage
  in case JD.decodeString messageDecoder strMessage of
    Ok message ->
      if message.event == "phx_reply" then
        socket.wrapper (handlePhxReply socket message)
      else if message.event == "phx_error" then
        socket.wrapper (handlePhxError socket message)
      else
        handleEvent socket message
    Err error ->
      socket.wrapper NoOp

handleEvent : Socket msg -> Message -> msg
handleEvent socket message =
  case Dict.get (message.event, message.topic) socket.events of
    Just decoder ->
      case JD.decodeValue decoder message.payload of
        Ok msg ->
          msg
        Err error ->
          socket.wrapper NoOp
    Nothing ->
      socket.wrapper NoOp  

handlePhxReply : Socket msg -> Message -> Msg
handlePhxReply socket message =
  if Dict.member message.topic socket.channels then
    SetChannelState message.topic ChannelJoined
  else
    NoOp

handlePhxError : Socket msg -> Message -> Msg
handlePhxError socket message =
  if Dict.member message.topic socket.channels then
    SetChannelState message.topic ChannelErrored
  else
    NoOp

-- COMMANDS

join : String -> JE.Value -> Socket msg -> (Socket msg, Cmd msg)
join channel params socket =
  if Dict.member channel socket.channels then
    ( socket, Cmd.none )
  else
    ( { socket | channels = Dict.insert channel ChannelJoining socket.channels }
    , WebSocket.send socket.path (joinChannelMessage channel params)
    )

joinChannelMessage : String -> JE.Value -> String
joinChannelMessage channel params =
  encodeMessage (Message channel "phx_join" params Nothing)

-- UPDATE

on : Event -> Channel -> JD.Decoder msg -> Socket msg -> Socket msg
on event channel decoder socket =
  { socket
  | events = Dict.insert (event, channel) decoder socket.events
  }

update : Msg -> Socket msg -> Socket msg
update msg socket =
  case msg of
    NoOp ->
      socket
    SetChannelState channel state ->
      { socket | channels = Dict.insert channel state socket.channels }
