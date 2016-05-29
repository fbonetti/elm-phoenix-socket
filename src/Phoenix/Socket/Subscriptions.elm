module Phoenix.Socket.Subscriptions exposing (listen)

import Phoenix.Socket.Model exposing (Model)
import Phoenix.Socket.Update exposing (Msg(..))
import Phoenix.Helpers exposing (Message, messageDecoder)
import Phoenix.Channel.Model as Channel
import WebSocket
import Json.Decode as JD exposing ((:=))
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
          ChannelError message.topic message.payload

        "phx_close" ->
          ChannelClose message.topic message.payload
        _ ->
          NoOp

    Nothing ->
      NoOp


handleEvent : Model msg -> Message -> Msg msg
handleEvent socket message =
  case Dict.get ( message.event, message.topic ) socket.events of
    Just payloadToMsg ->
      ExternalMsg (payloadToMsg message.payload)

    Nothing ->
      NoOp

replyDecoder : JD.Decoder (String, JD.Value)
replyDecoder =
  JD.object2 (,)
    ("status" := JD.string)
    ("response" := JD.value)

-- NOTE: This logic needs to be rewritten

handlePhxReply : Model msg -> Message -> Msg msg
handlePhxReply socket message =
  case JD.decodeValue replyDecoder message.payload of
    Ok ( status, response ) ->
      case status of
        "ok" ->
          PushOk message.ref response

        "error" ->
          PushError message.ref response

        _ ->
          NoOp

    Err error ->
      NoOp
