module Phoenix.Channel.Model exposing (Model, State(..), init)

import Phoenix.Helpers exposing (emptyPayload)
import Json.Encode as JE

type alias Model msg =
  { name : String
  , payload : JE.Value
  , state : State
  , onClose : Maybe (JE.Value -> msg)
  , onError : Maybe (JE.Value -> msg)
  , onJoin : Maybe (JE.Value -> msg)
  , onJoinError : Maybe (JE.Value -> msg)
  }

type State
  = Closed
  | Errored
  | Joined
  | Joining
  | Leaving

init : String -> Model msg
init name =
  { name = name
  , payload = emptyPayload
  , state = Closed
  , onClose = Nothing
  , onError = Nothing
  , onJoin = Nothing
  , onJoinError = Nothing
  }
