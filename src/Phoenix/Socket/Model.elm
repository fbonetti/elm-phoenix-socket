module Phoenix.Socket.Model exposing (Model, init)

import Phoenix.Channel.Model as Channel
import Dict exposing (Dict)

type alias Model msg =
  { path : String
  , debug : Bool
  , channels : Dict String (Channel.Model msg)
  --, events : Dict ( String, String ) (Event msg)
  , ref : Int
  }

init : String -> Model msg
init path =
  Model path False (Dict.fromList []) 0
