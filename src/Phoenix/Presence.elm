module Phoenix.Presence
    exposing
        ( PresenceState
        , PresenceStateMetaWrapper
        , PresenceStateMetaValue
        , PresenceDiff
        , syncState
        , syncDiff
        , presenceStateDecoder
        , presenceDiffDecoder
        , listDefault
        , list
        )

{-|

@docs PresenceState, PresenceStateMetaWrapper, PresenceStateMetaValue, PresenceDiff, syncState, syncDiff, presenceStateDecoder, presenceDiffDecoder, listDefault, list

-}

import Dict exposing (Dict)
import Json.Decode as JD


-- TYPES


{-| Represents the Phoenix Presence State
-}
type alias PresenceState a =
    Dict String (PresenceStateMetaWrapper a)


{-| Represents the metas for a given Presence key
-}
type alias PresenceStateMetaWrapper a =
    { metas : List (PresenceStateMetaValue a) }


{-| Represents the payload for a given meta value
-}
type alias PresenceStateMetaValue a =
    { phx_ref : String
    , payload : a
    }


{-| Represents a diff of presence states
-}
type alias PresenceDiff a =
    { leaves : PresenceState a
    , joins : PresenceState a
    }



-- Json Decoders


{-| Decodes a PresenceDiff, parameterized on the type of your application-defined payload
-}
presenceDiffDecoder : JD.Decoder a -> JD.Decoder (PresenceDiff a)
presenceDiffDecoder payloadDecoder =
    JD.map2 PresenceDiff
        (JD.field "leaves" <| presenceStateDecoder payloadDecoder)
        (JD.field "joins" <| presenceStateDecoder payloadDecoder)


{-| Decodes a PresenceState, parameterized on the type of your application-defined payload
-}
presenceStateDecoder : JD.Decoder a -> JD.Decoder (PresenceState a)
presenceStateDecoder payloadDecoder =
    JD.dict (presenceStateMetaWrapperDecoder payloadDecoder)


{-| Decodes a PresenceStateMetaWrapper, parameterized on the type of your application-defined payload
-}
presenceStateMetaWrapperDecoder : JD.Decoder a -> JD.Decoder (PresenceStateMetaWrapper a)
presenceStateMetaWrapperDecoder payloadDecoder =
    JD.map PresenceStateMetaWrapper
        (JD.field "metas" <| JD.list (presenceStateMetaDecoder payloadDecoder))


{-| Decodes a PresenceStateMetaValue, parameterized on the type of your application-defined payload
-}
presenceStateMetaDecoder : JD.Decoder a -> JD.Decoder (PresenceStateMetaValue a)
presenceStateMetaDecoder payloadDecoder =
    let
        createFinalRecord phxRef payload =
          JD.succeed (PresenceStateMetaValue phxRef payload)

        decodeWithPhxRef phxRef =
          JD.andThen (createFinalRecord phxRef) payloadDecoder
    in
        JD.andThen decodeWithPhxRef (JD.field "phx_ref" JD.string)



-- API


{-| Return the metas for a given PresenceState
-}
listDefault : PresenceState a -> List (PresenceStateMetaWrapper a)
listDefault =
    Dict.values


{-| Return the metas for a given PresenceState, mapped through a provided mapping function
-}
list : (String -> PresenceStateMetaWrapper a -> Maybe b) -> PresenceState a -> List (Maybe b)
list mapper =
    Dict.map mapper >> Dict.values


{-| Synchronize state - this is a really silly function presently, because this client does not provide callbacks as they make little-to-no sense in Elm.
-}
syncState : PresenceState a -> PresenceState a -> PresenceState a
syncState newState state =
    newState


{-| Synchronize a diff using the existing state
-}
syncDiff : PresenceDiff a -> PresenceState a -> PresenceState a
syncDiff diff state =
    let
        mergeLeaves : PresenceState a -> String -> PresenceStateMetaWrapper a -> PresenceStateMetaWrapper a
        mergeLeaves leaves key currentMetaWrapper =
            case Dict.get key leaves of
                Nothing ->
                    currentMetaWrapper

                Just leaveValues ->
                    let
                        leftRefs =
                            List.map .phx_ref leaveValues.metas
                    in
                        currentMetaWrapper.metas
                            |> List.filter
                                (\metaValue ->
                                    not (List.any (\phx_ref -> metaValue.phx_ref == phx_ref) leftRefs)
                                )
                            |> PresenceStateMetaWrapper

        mergeJoins : PresenceState a -> PresenceState a -> PresenceState a
        mergeJoins left right =
            let
                inBoth : String -> PresenceStateMetaWrapper a -> PresenceStateMetaWrapper a -> PresenceState a -> PresenceState a
                inBoth key leftValue rightValue acc =
                    Dict.insert key (PresenceStateMetaWrapper (leftValue.metas ++ rightValue.metas)) acc
            in
                merge Dict.insert
                    inBoth
                    Dict.insert
                    left
                    right
                    Dict.empty
    in
        state
            |> mergeJoins diff.joins
            |> Dict.map (mergeLeaves diff.leaves)
            |> Dict.filter (\_ metaWrapper -> metaWrapper.metas /= [])


merge :
    (comparable -> a -> result -> result)
    -> (comparable -> a -> b -> result -> result)
    -> (comparable -> b -> result -> result)
    -> Dict comparable a
    -> Dict comparable b
    -> result
    -> result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
    let
        stepState rKey rValue ( list, result ) =
            case list of
                [] ->
                    ( list, rightStep rKey rValue result )

                ( lKey, lValue ) :: rest ->
                    if lKey < rKey then
                        stepState rKey rValue ( rest, leftStep lKey lValue result )
                    else if lKey > rKey then
                        ( list, rightStep rKey rValue result )
                    else
                        ( rest, bothStep lKey lValue rValue result )

        ( leftovers, intermediateResult ) =
            Dict.foldl stepState ( Dict.toList leftDict, initialResult ) rightDict
    in
        List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers
