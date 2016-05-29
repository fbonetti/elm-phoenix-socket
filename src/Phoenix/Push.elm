module Phoenix.Push exposing (Push, init, withPayload, onError, onOk)

import Phoenix.Helpers exposing (emptyPayload)
import Json.Encode as JE

type alias Push msg =
  { event : String
  , channel : String
  , payload : JE.Value
  , onOk : Maybe (JE.Value -> msg)
  , onError : Maybe (JE.Value -> msg)
  }

init : String -> String -> Push msg
init event channel =
  Push event channel emptyPayload Nothing Nothing

withPayload : JE.Value -> Push msg -> Push msg
withPayload payload push =
  { push | payload = payload }

onOk : (JE.Value -> msg) -> Push msg -> Push msg
onOk valueToMsg push =
  { push | onOk = Just valueToMsg }

onError : (JE.Value -> msg) -> Push msg -> Push msg
onError valueToMsg push =
  { push | onError = Just valueToMsg }
