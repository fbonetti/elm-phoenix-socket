module Phoenix.Socket
  exposing
    ( Socket
    , Msg
    , Message
    , init
    , join
    , joinWithOptions
    , defaultChannelOptions
    , leave
    , update
    , on
    , off
    , listen
    , push
    , withDebug
    )

{-| This library is a pure Elm implementation of the Phoenix.Socket library
that comes bundled with the Phoenix web framework. It aims to abstract away
the more tedious bits of communicating with Phoenix, such as joining channels,
leaving channels, registering event handlers, and handling errors.

# Socket
@docs Socket, Msg, Message, init, update, listen, push, withDebug

# Channels
@docs join, joinWithOptions, defaultChannelOptions, leave

# Events
@docs on, off

-}

import Dict exposing (Dict)
import Dict.Extra
import WebSocket
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))
import Result

{-| Initializes a `Socket` with the given path
-}

init : String -> Socket msg
init path =
  Socket path (Dict.fromList []) (Dict.fromList []) 0 False


initChannel : ChannelState -> Channel msg
initChannel state =
  Channel state Nothing Nothing Nothing

{-| By default, onJoin, onClose, and onError are set to `Nothing`
-}

defaultChannelOptions : ChannelOptions msg
defaultChannelOptions =
  { onJoin = Nothing
  , onClose = Nothing
  , onError = Nothing
  }

{-| Stores channels, event handlers, and configuration options
-}

type alias Socket msg =
  { path : String
  , channels : Dict String (Channel msg)
  , events : Dict ( String, String ) (Event msg)
  , ref : Int
  , debug : Bool
  }


type alias Channel msg =
  { state : ChannelState
  , onJoin : Maybe (JE.Value -> msg)
  , onClose : Maybe (JE.Value -> msg)
  , onError : Maybe (JE.Value -> msg)
  }


type alias Event msg =
  { decoder : JD.Decoder msg
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

{-|-}
type Msg msg
  = NoOp
  | ExternalMsg msg
  | HandleChannelError String (Cmd msg)
  | SetChannelState String ChannelState

{-|-}
type alias Message =
  { topic : String
  , event : String
  , payload : JD.Value
  , ref : Maybe Int
  }



-- DECODERS / ENCODERS


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
messageEncoder { topic, event, payload, ref } =
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



-- HELPERS

-- SUBSCRIPTIONS

{-| Listens for phoenix messages and converts them into type `msg`
-}
listen : (Msg msg -> msg) -> Socket msg -> Sub msg
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


phoenixMessages : Socket msg -> Sub (Maybe Message)
phoenixMessages socket =
  WebSocket.listen socket.path (debugIfEnabled socket >> decodeMessage)

debugIfEnabled : Socket msg -> String -> String
debugIfEnabled socket =
  if socket.debug then
    Debug.log "phx_message"
  else
    identity

decodeMessage : String -> Maybe Message
decodeMessage =
  JD.decodeString messageDecoder >> Result.toMaybe

externalMsgs : Socket msg -> Sub (Msg msg)
externalMsgs socket =
  Sub.map (mapExternalMsgs socket) (phoenixMessages socket)

mapExternalMsgs : Socket msg -> Maybe Message -> Msg msg
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


internalMsgs : Socket msg -> Sub (Msg msg)
internalMsgs socket =
  Sub.map (mapInternalMsgs socket) (phoenixMessages socket)

mapInternalMsgs : Socket msg -> Maybe Message -> Msg msg
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


handleEvent : Socket msg -> Message -> Msg msg
handleEvent socket message =
  case Dict.get ( message.event, message.topic ) socket.events of
    Just { decoder, onError } ->
      case (JD.decodeValue decoder message.payload) of
        Ok msg ->
          ExternalMsg msg
        Err error ->
          ExternalMsg (onError error)

    Nothing ->
      NoOp


statusDecoder : JD.Decoder String
statusDecoder =
  JD.at [ "status" ] JD.string


handlePhxReply : Socket msg -> Message -> Msg msg
handlePhxReply socket message =
  case JD.decodeValue statusDecoder message.payload of
    Ok status ->
      case status of
        "ok" ->
          SetChannelState message.topic ChannelJoined

        _ ->
          if Dict.member message.topic socket.channels then
            SetChannelState message.topic ChannelErrored
          else
            NoOp

    Err error ->
      NoOp


handlePhxError : Socket msg -> Message -> Msg msg
handlePhxError socket message =
  SetChannelState message.topic ChannelErrored


handlePhxClose : Socket msg -> Message -> Msg msg
handlePhxClose socket message =
  SetChannelState message.topic ChannelClosed



-- PHOENIX COMMANDS

{-| Joins a channel with the given payload

    payload = Json.Encode.object [ ("user_id", Json.Encode.string "123") ]
    (phxSocket, phxCmd) = join "rooms:lobby" payload socket

-}
join : String -> JE.Value -> Socket msg -> ( Socket msg, Cmd msg )
join channelName payload socket =
  joinWithOptions channelName payload defaultChannelOptions socket

{-| Joins a channel with addition options

    payload = Json.Encode.object [ ("user_id", Json.Encode.string "123") ]
    channelOptions = { defaultChannelOptions | onError = Just HandleError }
    (phxSocket, phxCmd) = join "rooms:lobby" payload channelOptions socket

-}
joinWithOptions : String -> JE.Value -> ChannelOptions msg -> Socket msg -> ( Socket msg, Cmd msg )
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
updateChannelWithOptions { onJoin, onClose, onError } channel =
  { channel
    | onJoin = onJoin
    , onClose = onClose
    , onError = onError
  }

{-| Leaves a channel

    (phxSocket, phxCmd) = leave "rooms:lobby" socket

-}
leave : String -> Socket msg -> ( Socket msg, Cmd msg )
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

{-| Register an event handler

  socket
    |> on "new:msg" "rooms:lobby" chatMessageDecoder errorToMsg
    |> on "alert:msg" "rooms:lobby" alertMessageDecoder errorToMsg

-}
on : String -> String -> JD.Decoder msg -> (String -> msg) -> Socket msg -> Socket msg
on eventName channelName decoder onError socket =
  { socket
    | events = Dict.insert ( eventName, channelName ) (Event decoder onError) socket.events
  }

{-| Remove an event handler

  socket
    |> off "new:msg" "rooms:lobby"
    |> off "alert:msg" "rooms:lobby"

-}
off : String -> String -> Socket msg -> Socket msg
off eventName channelName socket =
  { socket
    | events = Dict.remove ( eventName, channelName ) socket.events
  }

{-| Send a message

  push "rooms:lobby" "new:msg" payload socket

-}
push : String -> String -> JE.Value -> Socket msg -> Cmd msg
push topic event payload socket =
  Message topic event payload (Just socket.ref)
    |> encodeMessage
    |> WebSocket.send socket.path



-- UPDATE

{-| Updates the `Socket` in order to keep track of channel state
-}
update : Msg msg -> Socket msg -> ( Socket msg, Cmd msg )
update msg sock =
  let
    socket =
      { sock | ref = sock.ref + 1 }
  in
    case msg of
      SetChannelState channelName state ->
        ( { socket
            | channels = Dict.Extra.updateIfExists channelName (setState state) socket.channels
          }
        , Cmd.none
        )

      HandleChannelError channelName cmd ->
        ( { socket
            | channels = Dict.Extra.updateIfExists channelName (setState ChannelErrored) socket.channels
          }
        , cmd
        )

      ExternalMsg msg ->
        ( socket, Cmd.none )

      NoOp ->
        ( socket, Cmd.none )


setState : ChannelState -> Channel msg -> Channel msg
setState state channel =
  { channel | state = state }

{-| When enabled, prints all incoming Phoenix messages to the console

  socket
    |> withDebug

-}
withDebug : Socket msg -> Socket msg
withDebug socket =
  { socket | debug = True }
