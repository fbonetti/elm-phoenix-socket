module Phoenix.Socket exposing (Socket, Msg, init, update, withDebug, join, leave, push, on, off, listen, withoutHeartbeat, withHeartbeatInterval, map)

{-|

# Socket
@docs Socket, Msg, init, withDebug, withoutHeartbeat, withHeartbeatInterval, update, listen, map

# Channels
@docs join, leave

# Events
@docs on, off

# Sending messages
@docs push

-}

import Phoenix.Channel as Channel exposing (Channel, setState)
import Phoenix.Push as Push exposing (Push)
import Phoenix.Helpers exposing (Message, messageDecoder, encodeMessage, emptyPayload)
import Dict exposing (Dict)
import WebSocket
import Json.Encode as JE
import Json.Decode as JD exposing (field)
import Maybe exposing (andThen)
import Time exposing (Time, every, second)


{-| Stores channels, event handlers, and configuration options
-}
type alias Socket msg =
    { path : String
    , debug : Bool
    , channels : Dict String (Channel msg)
    , events : Dict ( String, String ) (JE.Value -> msg)
    , pushes : Dict Int (Push msg)
    , ref : Int
    , heartbeatIntervalSeconds : Float
    , withoutHeartbeat : Bool
    }


{-| -}
type Msg msg
    = NoOp
    | ExternalMsg msg
    | ChannelErrored String
    | ChannelClosed String
    | ChannelJoined String
    | ReceiveReply String Int
    | Heartbeat Time


{-| Initializes a `Socket` with the given path
-}
init : String -> Socket msg
init path =
    { path = path
    , debug = False
    , channels = Dict.fromList []
    , events = Dict.fromList []
    , pushes = Dict.fromList []
    , ref = 0
    , heartbeatIntervalSeconds = 30
    , withoutHeartbeat = False
    }


{-| -}
update : Msg msg -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
update msg socket =
    case msg of
        ChannelErrored channelName ->
            let
                channels =
                    Dict.update channelName (Maybe.map (setState Channel.Errored)) socket.channels

                socket_ =
                    { socket | channels = channels }
            in
                ( socket_, Cmd.none )

        ChannelClosed channelName ->
            case Dict.get channelName socket.channels of
                Just channel ->
                    let
                        channels =
                            Dict.insert channelName (setState Channel.Closed channel) socket.channels

                        pushes =
                            Dict.remove channel.joinRef socket.pushes

                        socket_ =
                            { socket | channels = channels, pushes = pushes }
                    in
                        ( socket_, Cmd.none )

                Nothing ->
                    ( socket, Cmd.none )

        ChannelJoined channelName ->
            case Dict.get channelName socket.channels of
                Just channel ->
                    let
                        channels =
                            Dict.insert channelName (setState Channel.Joined channel) socket.channels

                        pushes =
                            Dict.remove channel.joinRef socket.pushes

                        socket_ =
                            { socket | channels = channels, pushes = pushes }
                    in
                        ( socket_, Cmd.none )

                Nothing ->
                    ( socket, Cmd.none )

        Heartbeat _ ->
            heartbeat socket

        _ ->
            ( socket, Cmd.none )


{-| When enabled, prints all incoming Phoenix messages to the console
-}
withDebug : Socket msg -> Socket msg
withDebug socket =
    { socket | debug = True }


{-| Sends the heartbeat every interval in seconds

    Default is 30 seconds
-}
withHeartbeatInterval : Float -> Socket msg -> Socket msg
withHeartbeatInterval intervalSeconds socket =
    { socket | heartbeatIntervalSeconds = intervalSeconds }


{-| Turns off the heartbeat
-}
withoutHeartbeat : Socket msg -> Socket msg
withoutHeartbeat socket =
    { socket | withoutHeartbeat = True }


{-| Joins a channel

    payload = Json.Encode.object [ ("user_id", Json.Encode.string "123") ]
    channel = Channel.init "rooms:lobby" |> Channel.withPayload payload
    (socket_, cmd) = join channel socket

-}
join : Channel msg -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
join channel socket =
    case Dict.get channel.name socket.channels of
        Just { state } ->
            if state == Channel.Joined || state == Channel.Joining then
                ( socket, Cmd.none )
            else
                joinChannel channel socket

        Nothing ->
            joinChannel channel socket


joinChannel : Channel msg -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
joinChannel channel socket =
    let
        push_ =
            Push "phx_join" channel.name channel.payload channel.onJoin channel.onJoinError

        channel_ =
            { channel | state = Channel.Joining, joinRef = socket.ref }

        socket_ =
            { socket
                | channels = Dict.insert channel.name channel_ socket.channels
            }
    in
        push push_ socket_


{-| Leaves a channel

    (socket_, cmd) = leave "rooms:lobby" socket

-}
leave : String -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
leave channelName socket =
    case Dict.get channelName socket.channels of
        Just channel ->
            if channel.state == Channel.Joining || channel.state == Channel.Joined then
                let
                    push_ =
                        Push.init "phx_leave" channel.name

                    channel_ =
                        { channel | state = Channel.Leaving, leaveRef = socket.ref }

                    socket_ =
                        { socket | channels = Dict.insert channelName channel_ socket.channels }
                in
                    push push_ socket_
            else
                ( socket, Cmd.none )

        Nothing ->
            ( socket, Cmd.none )


heartbeat : Socket msg -> ( Socket msg, Cmd (Msg msg) )
heartbeat socket =
    let
        push_ =
            Push.init "heartbeat" "phoenix"
    in
        push push_ socket


{-| Pushes a message

    push_ = Phoenix.Push.init "new:msg" "rooms:lobby"
    (socket_, cmd) = push push_ socket

-}
push : Push msg -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
push push_ socket =
    ( { socket
        | pushes = Dict.insert socket.ref push_ socket.pushes
        , ref = socket.ref + 1
      }
    , send socket push_.event push_.channel push_.payload
    )


{-| Registers an event handler

    socket
      |> on "new:msg" "rooms:lobby" ReceiveChatMessage
      |> on "alert:msg" "rooms:lobby" ReceiveAlertMessage

-}
on : String -> String -> (JE.Value -> msg) -> Socket msg -> Socket msg
on eventName channelName onReceive socket =
    { socket
        | events = Dict.insert ( eventName, channelName ) onReceive socket.events
    }


{-| Removes an event handler

    socket
      |> off "new:msg" "rooms:lobby"
      |> off "alert:msg" "rooms:lobby"

-}
off : String -> String -> Socket msg -> Socket msg
off eventName channelName socket =
    { socket
        | events = Dict.remove ( eventName, channelName ) socket.events
    }


send : Socket msg -> String -> String -> JE.Value -> Cmd (Msg msg)
send { path, ref } event channel payload =
    sendMessage path (Message event channel payload (Just ref))


sendMessage : String -> Message -> Cmd (Msg msg)
sendMessage path message =
    WebSocket.send path (encodeMessage message)



-- SUBSCRIPTIONS


{-| Listens for phoenix messages and converts them into type `msg`
-}
listen : Socket msg -> (Msg msg -> msg) -> Sub msg
listen socket fn =
    (Sub.batch >> Sub.map (mapAll fn))
        [ internalMsgs socket
        , externalMsgs socket
        , heartbeatSubscription socket
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
    WebSocket.listen socket.path decodeMessage


debugIfEnabled : Socket msg -> String -> String
debugIfEnabled socket =
    if socket.debug then
        Debug.log "phx_message"
    else
        identity


decodeMessage : String -> Maybe Message
decodeMessage =
    JD.decodeString messageDecoder >> Result.toMaybe


heartbeatSubscription : Socket msg -> Sub (Msg msg)
heartbeatSubscription socket =
    if socket.withoutHeartbeat then
        Sub.none
    else
        Time.every (Time.second * socket.heartbeatIntervalSeconds) Heartbeat


internalMsgs : Socket msg -> Sub (Msg msg)
internalMsgs socket =
    Sub.map (mapInternalMsgs socket) (phoenixMessages socket)


mapInternalMsgs : Socket msg -> Maybe Message -> Msg msg
mapInternalMsgs socket maybeMessage =
    case maybeMessage of
        Just mess ->
            let
                message =
                    if socket.debug then
                        Debug.log "Phoenix message" mess
                    else
                        mess
            in
                case message.event of
                    "phx_reply" ->
                        handleInternalPhxReply socket message

                    "phx_error" ->
                        ChannelErrored message.topic

                    "phx_close" ->
                        ChannelClosed message.topic

                    _ ->
                        NoOp

        Nothing ->
            NoOp


handleInternalPhxReply : Socket msg -> Message -> Msg msg
handleInternalPhxReply socket message =
    let
        msg =
            Result.toMaybe (JD.decodeValue replyDecoder message.payload)
                |> andThen
                    (\( status, response ) ->
                        message.ref
                            |> andThen
                                (\ref ->
                                    Dict.get message.topic socket.channels
                                        |> andThen
                                            (\channel ->
                                                if status == "ok" then
                                                    if ref == channel.joinRef then
                                                        Just (ChannelJoined message.topic)
                                                    else if ref == channel.leaveRef then
                                                        Just (ChannelClosed message.topic)
                                                    else
                                                        Nothing
                                                else
                                                    Nothing
                                            )
                                )
                    )
    in
        Maybe.withDefault NoOp msg


externalMsgs : Socket msg -> Sub (Msg msg)
externalMsgs socket =
    Sub.map (mapExternalMsgs socket) (phoenixMessages socket)


mapExternalMsgs : Socket msg -> Maybe Message -> Msg msg
mapExternalMsgs socket maybeMessage =
    case maybeMessage of
        Just message ->
            case message.event of
                "phx_reply" ->
                    handlePhxReply socket message

                "phx_error" ->
                    let
                        channel =
                            Dict.get message.topic socket.channels

                        onError =
                            channel |> andThen .onError

                        msg =
                            Maybe.map (\f -> (ExternalMsg << f) message.payload) onError
                    in
                        Maybe.withDefault NoOp msg

                "phx_close" ->
                    let
                        channel =
                            Dict.get message.topic socket.channels

                        onClose =
                            channel |> andThen .onClose

                        msg =
                            Maybe.map (\f -> (ExternalMsg << f) message.payload) onClose
                    in
                        Maybe.withDefault NoOp msg

                _ ->
                    handleEvent socket message

        Nothing ->
            NoOp


replyDecoder : JD.Decoder ( String, JD.Value )
replyDecoder =
    JD.map2 (,)
        (field "status" JD.string)
        (field "response" JD.value)


handlePhxReply : Socket msg -> Message -> Msg msg
handlePhxReply socket message =
    let
        msg =
            Result.toMaybe (JD.decodeValue replyDecoder message.payload)
                |> andThen
                    (\( status, response ) ->
                        message.ref
                            |> andThen
                                (\ref ->
                                    Dict.get ref socket.pushes
                                        |> andThen
                                            (\push ->
                                                case status of
                                                    "ok" ->
                                                        Maybe.map (\f -> (ExternalMsg << f) response) push.onOk

                                                    "error" ->
                                                        Maybe.map (\f -> (ExternalMsg << f) response) push.onError

                                                    _ ->
                                                        Nothing
                                            )
                                )
                    )
    in
        Maybe.withDefault NoOp msg


handleEvent : Socket msg -> Message -> Msg msg
handleEvent socket message =
    case Dict.get ( message.event, message.topic ) socket.events of
        Just payloadToMsg ->
            ExternalMsg (payloadToMsg message.payload)

        Nothing ->
            NoOp

{-| -}
map : (msg1 -> msg2) -> Socket msg1 -> Socket msg2
map fn socket = 
    { socket
    | channels = Dict.map (\_ channel -> Channel.map fn channel) socket.channels
    , events  = Dict.map (\_ event -> fn << event) socket.events
    , pushes = Dict.map (\_ push -> Push.map fn push) socket.pushes }
