module Phoenix.Channel.Update exposing (onError, onJoin, onClose)

import Phoenix.Channel.Model exposing (Model, State)
import Json.Encode as JE

withPayload : JE.Value -> Model msg -> Model msg
withPayload payload channel =
  { channel | payload = payload }

onError : (JE.Value -> msg) -> Model msg -> Model msg
onError valueToMsg channel =
  { channel | onError = Just valueToMsg }

onJoin : (JE.Value -> msg) -> Model msg -> Model msg
onJoin valueToMsg channel =
  { channel | onJoin = Just valueToMsg }

onClose : (JE.Value -> msg) -> Model msg -> Model msg
onClose valueToMsg channel =
  { channel | onClose = Just valueToMsg }

setState : State -> Model msg -> Model msg
setState state channel =
  { channel | state = state }
