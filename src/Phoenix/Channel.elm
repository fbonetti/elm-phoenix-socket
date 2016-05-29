module Phoenix.Channel exposing (Channel, State(..), init, withPayload, onError, onClose, onJoin, onJoinError, setState)

import Phoenix.Helpers exposing (emptyPayload)
import Json.Encode as JE

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

type State
  = Closed
  | Errored
  | Joined
  | Joining
  | Leaving

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

withPayload : JE.Value -> Channel msg -> Channel msg
withPayload payload channel =
  { channel | payload = payload }

onError : (JE.Value -> msg) -> Channel msg -> Channel msg
onError valueToMsg channel =
  { channel | onError = Just valueToMsg }

onClose : (JE.Value -> msg) -> Channel msg -> Channel msg
onClose valueToMsg channel =
  { channel | onClose = Just valueToMsg }

onJoin : (JE.Value -> msg) -> Channel msg -> Channel msg
onJoin valueToMsg channel =
  { channel | onJoin = Just valueToMsg }

onJoinError : (JE.Value -> msg) -> Channel msg -> Channel msg
onJoinError valueToMsg channel =
  { channel | onJoinError = Just valueToMsg }

setState : State -> Channel msg -> Channel msg
setState state channel =
  { channel | state = state }
