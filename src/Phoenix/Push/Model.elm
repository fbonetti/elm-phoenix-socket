module Phoenix.Push.Model exposing (Model, init)

import Phoenix.Helpers exposing (emptyPayload)
import Json.Encode as JE

type alias Model msg =
  { channel : String
  , event : String
  , payload : JE.Value
  , onError : Maybe (JE.Value -> msg)
  }

init : String -> String -> Model msg
init channel event =
  Model channel event emptyPayload Nothing
