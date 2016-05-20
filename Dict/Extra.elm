module Dict.Extra exposing (insertUnlessExists, updateIfExists)

import Dict exposing (Dict)

insertUnlessExists : comparable -> v -> Dict comparable v -> Dict comparable v
insertUnlessExists key value =
  Dict.update key (Maybe.withDefault value >> Just)

updateIfExists : comparable -> (v -> v) -> Dict comparable v -> Dict comparable v
updateIfExists key fn =
  Dict.update key (Maybe.map fn)
