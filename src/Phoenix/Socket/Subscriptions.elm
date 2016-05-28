module Phoenix.Socket.Subscriptions exposing (listen)

import Phoenix.Socket.Model exposing (Model)
import Phoenix.Socket.Update exposing (Msg(..))
import Phoenix.Helpers exposing (Message, messageDecoder)
import Phoenix.Channel.Model as Channel
import WebSocket
import Json.Decode as JD
import Dict

listen : (Msg msg -> msg) -> Model msg -> Sub msg
listen fn socket =
  (Sub.batch >> Sub.map (mapAll fn))
    [ internalMsgs socket
    , externalMsgs socket
    ]

mapAll : (Msg msg -> msg) -> Msg msg -> msg
mapAll fn internalMsg =
  case internalMsg of
    ExternalMsg msg ->
      msg
    _ ->
      fn internalMsg

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

-- NOTE: This logic needs to be rewritten

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
