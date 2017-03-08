module Phoenix.Channel exposing (Channel, State(..), init, withPayload, onError, onClose, onJoin, onJoinError, setState, map)

{-|

@docs Channel, State, init, withPayload, onError, onClose, onJoin, onJoinError, setState, map

-}

import Phoenix.Helpers exposing (emptyPayload)
import Json.Encode as JE


{-| Represents a phoenix channel
-}
type alias Channel msg =
    { name : String
    , payload : JE.Value
    , state : State
    , onClose : Maybe (JE.Value -> msg)
    , onError : Maybe (JE.Value -> msg)
    , onJoin : Maybe (JE.Value -> msg)
    , onJoinError : Maybe (JE.Value -> msg)
    , joinRef : Int
    , leaveRef : Int
    }


{-| All possible states a channel can be in
-}
type State
    = Closed
    | Errored
    | Joined
    | Joining
    | Leaving


{-| Initializes a channel

    init "rooms:lobby"

-}
init : String -> Channel msg
init name =
    { name = name
    , payload = emptyPayload
    , state = Closed
    , onClose = Nothing
    , onError = Nothing
    , onJoin = Nothing
    , onJoinError = Nothing
    , joinRef = -1
    , leaveRef = -1
    }


{-| Attaches a payload that's used for authentication

    payload = JE.object [ ("user_id", JE.string "123") ]
    init "rooms:lobby"
      |> withPayload payload

-}
withPayload : JE.Value -> Channel msg -> Channel msg
withPayload payload channel =
    { channel | payload = payload }


{-| -}
onError : (JE.Value -> msg) -> Channel msg -> Channel msg
onError valueToMsg channel =
    { channel | onError = Just valueToMsg }


{-| -}
onClose : (JE.Value -> msg) -> Channel msg -> Channel msg
onClose valueToMsg channel =
    { channel | onClose = Just valueToMsg }


{-| -}
onJoin : (JE.Value -> msg) -> Channel msg -> Channel msg
onJoin valueToMsg channel =
    { channel | onJoin = Just valueToMsg }


{-| -}
onJoinError : (JE.Value -> msg) -> Channel msg -> Channel msg
onJoinError valueToMsg channel =
    { channel | onJoinError = Just valueToMsg }

{-| -}
map : (msg1 -> msg2) -> Channel msg1 -> Channel msg2
map fn channel = 
    { channel 
    | onClose = Maybe.map ((<<) fn) channel.onClose
    , onError = Maybe.map ((<<) fn) channel.onError
    , onJoin = Maybe.map ((<<) fn) channel.onJoin
    , onJoinError = Maybe.map ((<<) fn) channel.onJoinError }


{-| Sets the state of a channel. Internal use only.
-}
setState : State -> Channel msg -> Channel msg
setState state channel =
    { channel | state = state }
