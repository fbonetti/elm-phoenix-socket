module Main exposing (..)

import Html exposing (Html, h1, div, text, ul, li, input, form)
import Html.Attributes exposing (type', value)
import Html.Events exposing (onInput, onSubmit)
import Html.App
import WebSocket
import Platform.Cmd
import Phoenix.Socket
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))

-- MAIN

-- WebSocket.send socketServer ("{\"topic\":\"rooms:lobby\",\"event\":\"new:msg\",\"payload\":{\"user\":\"frank\",\"body\":\"" ++ model.newMessage ++ "\"},\"ref\":null}")

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
  | PhoenixMsg Phoenix.Socket.Msg
  | ReceivePhxMessage ChatMessage
  | NoOp


type alias Model =
  { newMessage : String
  , messages : List String
  , phxSocket : Phoenix.Socket.Socket Msg
  }

initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket =
  Phoenix.Socket.init socketServer PhoenixMsg
    |> Phoenix.Socket.on "new:msg" "rooms:lobby" receivePhxMessageDecoder

initModel : Model
initModel =
  Model "" [] initPhxSocket


init : ( Model, Cmd Msg )
init =
  ( initModel, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  --WebSocket.listen socketServer ReceiveMessage
  Phoenix.Socket.listen model.phxSocket


-- COMMANDS


-- PHOENIX STUFF

type alias ChatMessage =
  { user : String
  , body : String
  }

receivePhxMessageDecoder : JD.Decoder Msg
receivePhxMessageDecoder =
  JD.object2 ChatMessage
    ("user" := JD.string)
    ("body" := JD.string)
  |> JD.map ReceivePhxMessage

-- UPDATE

userParams : JE.Value
userParams =
  JE.object [ ("user_id", JE.string "123") ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ReceiveMessage str ->
      ( { model | messages = str :: model.messages }
      , Cmd.none
      )

    PhoenixMsg msg ->
      ( { model | phxSocket = Phoenix.Socket.update msg model.phxSocket }
      , Cmd.none
      )

    SendMessage ->
      let
        (phxSocket, phxCmd) = Phoenix.Socket.join "rooms:lobby" userParams model.phxSocket
      in
        ( { model
          | newMessage = ""
          , phxSocket = phxSocket
          }
        , Cmd.batch
            [ WebSocket.send socketServer ("{\"topic\":\"rooms:lobby\",\"event\":\"new:msg\",\"payload\":{\"user\":\"frank\",\"body\":\"" ++ model.newMessage ++ "\"},\"ref\":null}")
            , phxCmd
            ]
        )

    SetNewMessage str ->
      ( { model | newMessage = str }
      , Cmd.none
      )

    ReceivePhxMessage chatMessage ->
      ( { model | messages = (chatMessage.user ++ ": " ++ chatMessage.body) :: model.messages }
      , Cmd.none
      )

    NoOp ->
      ( model, Cmd.none )


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Messages:" ]
    , text (toString model.phxSocket.channels)
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
