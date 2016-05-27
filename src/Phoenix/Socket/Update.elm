module Phoenix.Socket.Update exposing (update, join, leave)

import Phoenix.Socket.Model exposing (Model)
import Phoenix.Channel.Model as Channel
import Phoenix.Push.Model as Push
import Phoenix.Helpers exposing (Message, encodeMessage, emptyPayload)
import Dict
import WebSocket
import Json.Encode as JE

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
        , send socket channel.name "phx_join" channel.payload
        )

    Nothing ->
      ( { socket
          | channels =
              Dict.insert channel.name { channel | state = Channel.Joining } socket.channels
        }
      , send socket channel.name "phx_join" channel.payload
      )

leave : String -> Model msg -> ( Model msg, Cmd msg )
leave channelName socket =
  case Dict.get channelName socket.channels of
    Just channel ->
      ( { socket | channels = Dict.insert channelName { channel | state = Channel.Leaving } socket.channels }
      , send socket channelName "phx_leave" emptyPayload
      )

    Nothing ->
      ( socket, Cmd.none )

push : Push.Model msg -> Model msg -> Cmd a
push {channel, event, payload} socket =
  send socket channel event payload

send : Model msg -> String -> String -> JE.Value -> Cmd a
send {path,ref} channel event payload =
  sendMessage path (Message channel event payload (Just ref))

sendMessage : String -> Message -> Cmd a
sendMessage path message =
  WebSocket.send path (encodeMessage message)
