module Phoenix.Socket exposing (Socket, Msg, Message, init, join, joinWithOptions, leave, update, on, off, events, push)

import Dict exposing (Dict)
import WebSocket
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))
import Task

-- MODEL

init : String -> Socket msg
init path =
  Socket path (Dict.fromList []) (Dict.fromList [])

initChannel : ChannelState -> Channel msg
initChannel state =
  Channel state Nothing Nothing Nothing

defaultChannelOptions : ChannelOptions msg
defaultChannelOptions =
  { onJoin = Nothing
  , onClose = Nothing
  , onError = Nothing
  }

type alias Socket msg =
  { path : String
  , channels : Dict String (Channel msg)
  , events : Dict (String, String) (Event msg)
  }

type alias Channel msg =
  { state : ChannelState
  , onJoin : Maybe (JE.Value -> msg)
  , onClose : Maybe (JE.Value -> msg)
  , onError : Maybe (JE.Value -> msg)
  }

type alias Event msg =
  { decoder: JD.Decoder msg
  , onError : String -> msg
  }

type alias ChannelOptions msg =
  { onJoin : Maybe (JE.Value -> msg)
  , onClose : Maybe (JE.Value -> msg)
  , onError : Maybe (JE.Value -> msg)
  }

type ChannelState
  = ChannelClosed
  | ChannelErrored
  | ChannelJoined
  | ChannelJoining
  | ChannelLeaving

type Msg msg
  = NoOp
  | ExposeExternalMsg msg
  | DispatchMultipleMsgs (List (Msg msg))
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
    ExposeExternalMsg m ->
      m
    _ ->
      fn msg

handleMessage : Socket msg -> String -> Msg msg
handleMessage socket strMessage =
  case JD.decodeString messageDecoder strMessage of
    Ok message ->
      case message.event of
        "phx_reply" ->
          handlePhxReply socket message
        "phx_error" ->
          handlePhxError socket message
        "phx_close" ->
          handlePhxClose socket message
        _ ->
          handleEvent socket message

    Err error ->
      NoOp

handleEvent : Socket msg -> Message -> Msg msg
handleEvent socket message =
  case Dict.get (message.event, message.topic) socket.events of
    Just {decoder,onError} ->
      case JD.decodeValue decoder message.payload of
        Ok msg ->
          ExposeExternalMsg msg
        Err error ->
          ExposeExternalMsg (onError error)
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
          case Dict.get message.topic socket.channels of
            Just channel ->
              case channel.onError of
                Just onError ->
                  DispatchMultipleMsgs
                    [ ExposeExternalMsg (onError message.payload)
                    , SetChannelState message.topic ChannelErrored
                    ]
                Nothing ->
                  SetChannelState message.topic ChannelErrored
            Nothing ->
              NoOp
        _ ->
          NoOp
    Err error ->
      NoOp

handlePhxError : Socket msg -> Message -> Msg msg
handlePhxError socket message =
  SetChannelState message.topic ChannelErrored

handlePhxClose : Socket msg -> Message -> Msg msg
handlePhxClose socket message =
  SetChannelState message.topic ChannelClosed

-- COMMANDS

join : String -> JE.Value -> Socket msg -> (Socket msg, Cmd msg)
join channelName payload socket =
  joinWithOptions channelName payload defaultChannelOptions socket

joinWithOptions : String -> JE.Value -> ChannelOptions msg -> Socket msg -> (Socket msg, Cmd msg)
joinWithOptions channelName payload channelOptions socket =
  case Dict.get channelName socket.channels of
    Just channel ->
      if channel.state == ChannelJoining || channel.state == ChannelJoined then
        ( socket, Cmd.none )
      else
        ( { socket
          | channels =
              Dict.insert channelName (updateChannelWithOptions channelOptions channel) socket.channels
          }
        , push channelName "phx_join" payload socket
        )
    Nothing ->
      ( { socket
        | channels =
            Dict.insert channelName (updateChannelWithOptions channelOptions (initChannel ChannelJoining)) socket.channels
        }
      , push channelName "phx_join" payload socket
      )

updateChannelWithOptions : ChannelOptions msg -> Channel msg -> Channel msg
updateChannelWithOptions {onJoin,onClose,onError} channel =
  { channel
  | onJoin = onJoin
  , onClose = onClose
  , onError = onError
  }


leave : String -> Socket msg -> (Socket msg, Cmd msg)
leave channelName socket =
  case Dict.get channelName socket.channels of
    Just channel ->
      if channel.state == ChannelJoined || channel.state == ChannelJoining then
        ( { socket | channels = Dict.insert channelName { channel | state = ChannelLeaving } socket.channels }
        , push channelName "phx_leave" emptyPayload socket
        )
      else
        ( socket, Cmd.none )
    Nothing ->
      ( socket, Cmd.none )

push : String -> String -> JE.Value -> Socket msg -> Cmd msg
push topic event payload socket =
  Message topic event payload Nothing
    |> encodeMessage
    |> WebSocket.send socket.path

-- UPDATE

on : String -> String -> JD.Decoder msg -> (String -> msg) -> Socket msg -> Socket msg
on eventName channelName decoder onError socket =
  { socket
  | events = Dict.insert (eventName, channelName) (Event decoder onError) socket.events
  }

off : String -> String -> Socket msg -> Socket msg
off eventName channelName socket =
  { socket
  | events = Dict.remove (eventName, channelName) socket.events
  }

setState : ChannelState -> Channel msg -> Channel msg
setState state channel =
  { channel | state = state }

update : Msg msg -> Socket msg -> (Socket msg, Cmd (Msg msg))
update msg socket =
  case msg of
    SetChannelState channelName state ->
      ({ socket
      | channels = Dict.update channelName (Maybe.map (setState state)) socket.channels
      }
      , Cmd.none
      )
    DispatchMultipleMsgs msgs ->
      ( socket, dispatchMultipleMsgs msgs )
    ExposeExternalMsg msg ->
      ( socket, Cmd.none )
    NoOp ->
      ( socket, Cmd.none )


dispatchMultipleMsgs : List (Msg msg) -> Cmd (Msg msg)
dispatchMultipleMsgs =
  List.map dispatchMsg >> Cmd.batch

dispatchMsg : Msg msg -> Cmd (Msg msg)
dispatchMsg =
  Task.succeed >> Task.perform (always NoOp) identity
