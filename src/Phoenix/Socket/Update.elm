module Phoenix.Socket.Update exposing (Msg(..), update, join, leave)

import Phoenix.Socket.Model exposing (Model)
import Phoenix.Channel.Model as Channel
import Phoenix.Channel.Update exposing (setState)
import Phoenix.Push.Model as Push
import Phoenix.Helpers exposing (Message, encodeMessage, emptyPayload)
import Dict
import Dict.Extra
import WebSocket
import Json.Encode as JE

type Msg msg
  = NoOp
  | ExternalMsg msg
  | SetChannelState String Channel.State

update : Msg msg -> Model msg -> ( Model msg, Cmd a )
update msg socket =
  case msg of
    SetChannelState channelName state ->
      ( { socket
          | channels = Dict.Extra.updateIfExists channelName (setState state) socket.channels
        }
      , Cmd.none
      )

    ExternalMsg msg ->
      ( socket, Cmd.none )

    NoOp ->
      ( socket, Cmd.none )

{-| When enabled, prints all incoming Phoenix messages to the console
-}

withDebug : Model msg -> Model msg
withDebug socket =
  { socket | debug = True }

join : Channel.Model msg -> Model msg -> (Model msg, Cmd a)
join channel socket =
  -- A channel can only be joined once
  if Dict.member channel.name socket.channels then
    ( socket, Cmd.none )
  else
    rejoin channel socket

rejoin : Channel.Model msg -> Model msg -> (Model msg, Cmd a)
rejoin channel socket =
  case Dict.get channel.name socket.channels of
    Just {state} ->
      if state == Channel.Leaving then
        ( socket, Cmd.none )
      else
        let
          push' = Push.Model "phx_join" channel.name channel.payload channel.onJoin channel.onError
          sock =
            { socket
              | channels = Dict.insert channel.name (setState Channel.Joining channel) socket.channels
            }
        in
          push push' sock

    Nothing ->
      ( socket, Cmd.none )

leave : String -> Model msg -> ( Model msg, Cmd msg )
leave channelName socket =
  case Dict.get channelName socket.channels of
    Just channel ->
      ( { socket | channels = Dict.insert channelName (setState Channel.Leaving channel) socket.channels }
      , send socket "phx_leave" channelName emptyPayload
      )

    Nothing ->
      ( socket, Cmd.none )

push : Push.Model msg -> Model msg -> (Model msg, Cmd a)
push push' socket =
  ( { socket
      | pushes = Dict.insert socket.ref push' socket.pushes
      , ref = socket.ref + 1
    }
  , send socket push'.event push'.channel push'.payload
  )


on : String -> String -> (JE.Value -> msg) -> Model msg -> Model msg
on eventName channelName onReceive socket =
  { socket
    | events = Dict.insert ( eventName, channelName ) onReceive socket.events
  }

off : String -> String -> Model msg -> Model msg
off eventName channelName socket =
  { socket
    | events = Dict.remove ( eventName, channelName ) socket.events
  }

send : Model msg -> String -> String -> JE.Value -> Cmd a
send {path,ref} event channel payload =
  sendMessage path (Message event channel payload (Just ref))

sendMessage : String -> Message -> Cmd a
sendMessage path message =
  WebSocket.send path (encodeMessage message)
