module Phoenix.Channel.Model exposing (Model, State(..), init)

import Phoenix.Helpers exposing (emptyPayload)
import Json.Encode as JE

type alias Model msg =
  { name : String
  , payload : JE.Value
  , state : State
  , onClose : Maybe (JE.Value -> msg)
  , onError : Maybe (JE.Value -> msg)
  }

type State
  = Closed
  | Errored
  | Joined
  | Joining
  | Leaving

init : String -> Model msg
init name =
  Model name emptyPayload Closed Nothing Nothing
