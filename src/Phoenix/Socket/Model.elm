module Phoenix.Socket.Model exposing (Model, init)

import Phoenix.Channel.Model as Channel
import Phoenix.Push.Model as Push
import Dict exposing (Dict)
import Json.Encode as JE

type alias Model msg =
  { path : String
  , debug : Bool
  , channels : Dict String (Channel.Model msg)
  , events : Dict ( String, String ) (JE.Value -> msg)
  , pushes : Dict Int (Push.Model msg)
  , ref : Int
  }

init : String -> Model msg
init path =
  { path = path
  , debug = False
  , channels = Dict.fromList []
  , events = Dict.fromList []
  , pushes = Dict.fromList []
  , ref = 0
  }
