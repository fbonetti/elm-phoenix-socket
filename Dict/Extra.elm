module Dict.Extra exposing (insertUnlessExists)

import Dict exposing (Dict)

insertUnlessExists : comparable -> v -> Dict comparable v -> Dict comparable v
insertUnlessExists key value =
  Dict.update key (Maybe.withDefault value >> Just)