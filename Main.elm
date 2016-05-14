module Main exposing (..)

import Html exposing (Html, h1, div, text, ul, li, input, form)
import Html.Attributes exposing (type', value)
import Html.Events exposing (onInput, onSubmit)
import Html.App
import WebSocket


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
socketServer = "ws://echo.websocket.org"


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
      , WebSocket.send socketServer model.newMessage
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
