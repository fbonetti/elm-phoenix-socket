module Phoenix.Push.Model exposing (Model, init)

import Phoenix.Helpers exposing (emptyPayload)
import Json.Encode as JE

type alias Model msg =
  { event : String
  , channel : String
  , payload : JE.Value
  , onOk : Maybe (JE.Value -> msg)
  , onError : Maybe (JE.Value -> msg)
  }

init : String -> String -> Model msg
init event channel =
  Model event channel emptyPayload Nothing Nothing
