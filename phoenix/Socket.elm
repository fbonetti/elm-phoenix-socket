module Phoenix.Socket exposing (Socket, Msg, init, join, leave, update, on, events, send)

import Dict exposing (Dict)
import WebSocket
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))

-- MODEL

init : String -> Socket msg
init path =
  Socket path (Dict.fromList []) (Dict.fromList [])

initChannel : ChannelState -> Channel msg
initChannel state =
  Channel state Nothing

type alias Socket msg =
  { path : String
  , channels : Dict String (Channel msg)
  , events : Dict (String, String) (Event msg)
  }

type alias Channel msg =
  { state : ChannelState
  , onError : Maybe (String -> msg)
  }

type alias Event msg =
  { decoder: JD.Decoder msg
  , onError : Maybe (String -> msg)
  }

type ChannelState
    = ChannelClosed
    | ChannelErrored
    | ChannelJoined
    | ChannelJoining
    | ChannelLeaving

type Msg msg
    = NoOp
    | DispatchMessage msg
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

emptyPayload : JE.Value
emptyPayload =
  JE.object []

-- SUBSCRIPTIONS


events : (Msg msg -> msg) -> Socket msg -> Sub msg
events fn socket =
  WebSocket.listen socket.path (handleMessage socket)
    |> Sub.map (mapToExternalMsg fn)

mapToExternalMsg : (Msg msg -> msg) -> Msg msg -> msg
mapToExternalMsg fn msg =
  case msg of
    DispatchMessage m ->
      m
    _ ->
      fn msg

handleMessage : Socket msg -> String -> Msg msg
handleMessage socket strMessage =
  let a = Debug.log "rawMessage" strMessage
  in case JD.decodeString messageDecoder strMessage of
    Ok message ->
      case message.event of
        "phx_reply" ->
          handlePhxReply socket message
        "phx_error" ->
          handlePhxError socket message
        "phx_close" ->
          handlePhxClose message socket
        _ ->
          handleEvent socket message

    Err error ->
      NoOp

handleEvent : Socket msg -> Message -> Msg msg
handleEvent socket message =
  case Dict.get (message.event, message.topic) socket.events of
    Just {decoder} ->
      case JD.decodeValue decoder message.payload of
        Ok msg ->
          DispatchMessage msg
        Err error ->
          NoOp
    Nothing ->
      NoOp  

statusDecoder : JD.Decoder String
statusDecoder =
  JD.at ["status"] JD.string

handlePhxReply : Socket msg -> Message -> Msg msg
handlePhxReply socket message =
  case JD.decodeValue statusDecoder message.payload of
    Ok status ->
      case status of
        "ok" ->
          SetChannelState message.topic ChannelJoined
        "error" ->
          SetChannelState message.topic ChannelErrored
        _ ->
          NoOp
    Err error ->
      NoOp

handlePhxError : Socket msg -> Message -> Msg msg
handlePhxError socket message =
  SetChannelState message.topic ChannelErrored

handlePhxClose : Message -> Socket msg -> Msg msg
handlePhxClose message socket =
  SetChannelState message.topic ChannelClosed

-- COMMANDS

join : String -> JE.Value -> Socket msg -> (Socket msg, Cmd msg)
join channelName params socket =
  case Dict.get channelName socket.channels of
    Just channel ->
      if channel.state == ChannelJoining || channel.state == ChannelJoined then
        ( socket, Cmd.none )
      else
        ( { socket | channels = Dict.insert channelName { channel | state = ChannelJoining } socket.channels }
        , send channelName "phx_join" params socket
        )
    Nothing ->
      ( { socket | channels = Dict.insert channelName (initChannel ChannelJoining) socket.channels }
      , send channelName "phx_join" params socket
      )

leave : String -> Socket msg -> (Socket msg, Cmd msg)
leave channelName socket =
  case Dict.get channelName socket.channels of
    Just channel ->
      if channel.state == ChannelJoined || channel.state == ChannelJoining then
        ( { socket | channels = Dict.insert channelName { channel | state = ChannelLeaving } socket.channels }
        , send channelName "phx_leave" emptyPayload socket
        )
      else
        ( socket, Cmd.none )
    Nothing ->
      ( socket, Cmd.none )

send : String -> String -> JE.Value -> Socket msg -> Cmd msg
send topic event payload socket =
  Message topic event payload Nothing
    |> encodeMessage
    |> WebSocket.send socket.path

-- UPDATE

on : String -> String -> JD.Decoder msg -> Socket msg -> Socket msg
on eventName channelName decoder socket =
  { socket
  | events = Dict.insert (eventName, channelName) (Event decoder Nothing) socket.events
  }

setState : ChannelState -> Channel msg -> Channel msg
setState state channel =
  { channel | state = state }

update : Msg msg -> Socket msg -> Socket msg
update msg socket =
  case msg of
    SetChannelState channelName state ->
      { socket
      | channels = Dict.update channelName (Maybe.map (setState state)) socket.channels
      }
    DispatchMessage _ ->
      socket
    NoOp ->
      socket
