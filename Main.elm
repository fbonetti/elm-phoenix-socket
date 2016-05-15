module Main exposing (..)

import Html exposing (Html, h1, div, text, ul, li, input, form)
import Html.Attributes exposing (type', value)
import Html.Events exposing (onInput, onSubmit)
import Html.App
import WebSocket
import Platform.Cmd
import Json.Encode as JE
import Phoenix.Socket

-- MAIN


main : Program Never
main =
  Html.App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


-- CONSTANTS


socketServer : String
socketServer = "ws://phoenixchat.herokuapp.com/ws"


-- MODEL


type Msg
  = ReceiveMessage String
  | SendMessage
  | SetNewMessage String
  | NoOp


type alias Model =
  { newMessage : String
  , messages : List String
  }


initModel : Model
initModel =
  Model "" []


init : ( Model, Cmd Msg )
init =
  ( initModel, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen socketServer ReceiveMessage


-- COMMANDS


joinPhoenixChannel : String -> Cmd msg
joinPhoenixChannel topic =
  let
    payload = JE.object [ ("user_id", JE.int 123) ]
  in
    sendPhoenixPacket topic "phx_join" payload


-- PHOENIX STUFF

packetEncoder : String -> String -> JE.Value -> String
packetEncoder topic event payload =
  JE.object
    [ ( "topic", JE.string topic )
    , ( "event", JE.string event )
    , ( "payload", payload )
    , ( "ref", JE.null )
    ]
  |> JE.encode 0

sendPhoenixPacket : String -> String -> JE.Value -> Cmd msg
sendPhoenixPacket topic event payload =
  WebSocket.send socketServer (packetEncoder topic event payload)


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ReceiveMessage str ->
      ( { model | messages = str :: model.messages }
      , Cmd.none
      )

    SendMessage ->
      ( { model
        | messages = model.newMessage :: model.messages
        , newMessage = ""
        }
      , Platform.Cmd.batch
          [ WebSocket.send socketServer ("{\"topic\":\"rooms:lobby\",\"event\":\"new:msg\",\"payload\":{\"user\":\"frank\",\"body\":\"" ++ model.newMessage ++ "\"},\"ref\":null}")
          , snd (Phoenix.Socket.join "rooms:lobby" (JE.object [ ("user_id", JE.int 123) ]) (Phoenix.Socket.init socketServer))
          ]
      )

    SetNewMessage str ->
      ( { model | newMessage = str }
      , Cmd.none
      )

    NoOp ->
      ( model, Cmd.none )


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Messages:" ]
    , newMessageForm model
    , ul [] (List.map renderMessage model.messages)
    ]

newMessageForm : Model -> Html Msg
newMessageForm model =
  form [ onSubmit SendMessage ]
    [ input [ type' "text", value model.newMessage, onInput SetNewMessage ] []
    ]

renderMessage : String -> Html Msg
renderMessage str =
  li [] [ text str ]
