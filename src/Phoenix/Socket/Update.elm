module Phoenix.Socket.Update exposing (update, join, leave)

import Phoenix.Socket.Model exposing (Model)
import Phoenix.Channel.Model as Channel
import Phoenix.Push.Model as Push
import Phoenix.Helpers exposing (Message, messageDecoder, encodeMessage, emptyPayload)
import Dict
import WebSocket
import Json.Encode as JE
import Json.Decode as JD

type Msg msg
  = NoOp
  | ExternalMsg msg
  | SetChannelState String Channel.State

update : Msg -> Model msg -> (Model msg, Cmd a)
update msg socket =
  case msg of
    _ ->
      ( socket, Cmd.none )

{-| When enabled, prints all incoming Phoenix messages to the console
-}

withDebug : Model msg -> Model msg
withDebug socket =
  { socket | debug = True }

join : Channel.Model msg -> Model msg -> (Model msg, Cmd a)
join channel socket =
  case Dict.get channel.name socket.channels of
    Just {state} ->
      if state == Channel.Joining || state == Channel.Joined then
        ( socket, Cmd.none )
      else
        ( { socket
            | channels =
                Dict.insert channel.name { channel | state = Channel.Joining } socket.channels
          }
        , send socket "phx_join" channel.name channel.payload
        )

    Nothing ->
      ( { socket
          | channels =
              Dict.insert channel.name { channel | state = Channel.Joining } socket.channels
        }
      , send socket "phx_join" channel.name channel.payload
      )

leave : String -> Model msg -> ( Model msg, Cmd msg )
leave channelName socket =
  case Dict.get channelName socket.channels of
    Just channel ->
      ( { socket | channels = Dict.insert channelName { channel | state = Channel.Leaving } socket.channels }
      , send socket "phx_leave" channelName emptyPayload
      )

    Nothing ->
      ( socket, Cmd.none )

push : Push.Model msg -> Model msg -> (Model msg, Cmd a)
push push socket =
  ( { socket | pushes = Dict.insert socket.ref push socket.pushes }
  , send socket push.event push.channel push.payload
  )

send : Model msg -> String -> String -> JE.Value -> Cmd a
send {path,ref} event channel payload =
  sendMessage path (Message event channel payload (Just ref))

sendMessage : String -> Message -> Cmd a
sendMessage path message =
  WebSocket.send path (encodeMessage message)



phoenixMessages : Model msg -> Sub (Maybe Message)
phoenixMessages socket =
  WebSocket.listen socket.path (debugIfEnabled socket >> decodeMessage)

debugIfEnabled : Model msg -> String -> String
debugIfEnabled socket =
  if socket.debug then
    Debug.log "phx_message"
  else
    identity

decodeMessage : String -> Maybe Message
decodeMessage =
  JD.decodeString messageDecoder >> Result.toMaybe

externalMsgs : Model msg -> Sub (Msg msg)
externalMsgs socket =
  Sub.map (mapExternalMsgs socket) (phoenixMessages socket)

mapExternalMsgs : Model msg -> Maybe Message -> Msg msg
mapExternalMsgs socket maybeMessage =
  case maybeMessage of
    Just message ->
      case message.event of
        "phx_reply" ->
          NoOp
        "phx_error" ->
          NoOp
        "phx_close" ->
          NoOp
        _ ->
          handleEvent socket message

    Nothing ->
      NoOp


internalMsgs : Model msg -> Sub (Msg msg)
internalMsgs socket =
  Sub.map (mapInternalMsgs socket) (phoenixMessages socket)

mapInternalMsgs : Model msg -> Maybe Message -> Msg msg
mapInternalMsgs socket maybeMessage =
  case maybeMessage of
    Just message ->
      case message.event of
        "phx_reply" ->
          handlePhxReply socket message

        "phx_error" ->
          handlePhxError socket message

        "phx_close" ->
          handlePhxClose socket message
        _ ->
          NoOp

    Nothing ->
      NoOp


handleEvent : Model msg -> Message -> Msg msg
handleEvent socket message =
  case Dict.get ( message.event, message.topic ) socket.events of
    Just onOk ->
      ExternalMsg (onOk message.payload)

    Nothing ->
      NoOp


statusDecoder : JD.Decoder String
statusDecoder =
  JD.at [ "status" ] JD.string


handlePhxReply : Model msg -> Message -> Msg msg
handlePhxReply socket message =
  case JD.decodeValue statusDecoder message.payload of
    Ok status ->
      case status of
        "ok" ->
          SetChannelState message.topic Channel.Joined

        _ ->
          if Dict.member message.topic socket.channels then
            SetChannelState message.topic Channel.Errored
          else
            NoOp

    Err error ->
      NoOp


handlePhxError : Model msg -> Message -> Msg msg
handlePhxError socket message =
  SetChannelState message.topic Channel.Errored


handlePhxClose : Model msg -> Message -> Msg msg
handlePhxClose socket message =
  SetChannelState message.topic Channel.Closed
