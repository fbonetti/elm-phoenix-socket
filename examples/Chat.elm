module Chat exposing (..) --where

import Html exposing (Html, h1, div, text, ul, li, input, form, button, br)
import Html.Attributes exposing (type', value)
import Html.Events exposing (onInput, onSubmit, onClick)
import Html.App
import Platform.Cmd
import Phoenix.SocketOld
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))

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
  | PhoenixMsg (Phoenix.SocketOld.Msg Msg)
  | ReceivePhxMessage ChatMessage
  | JoinChannel
  | LeaveChannel
  | NoOp


type alias Model =
  { newMessage : String
  , messages : List String
  , phxSocket : Phoenix.SocketOld.Socket Msg
  }

initPhxSocket : Phoenix.SocketOld.Socket Msg
initPhxSocket =
  Phoenix.SocketOld.init socketServer
    |> Phoenix.SocketOld.withDebug
    |> Phoenix.SocketOld.on "new:msg" "rooms:lobby" receivePhxMessageDecoder (always NoOp)

initModel : Model
initModel =
  Model "" [] initPhxSocket


init : ( Model, Cmd Msg )
init =
  ( initModel, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Phoenix.SocketOld.listen PhoenixMsg model.phxSocket

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
      let
        ( phxSocket, phxCmd ) = Phoenix.SocketOld.update msg model.phxSocket
      in
        ( { model | phxSocket = phxSocket }, phxCmd )

    SendMessage ->
      let
        payload = (JE.object [ ("user", JE.string "frank"), ("body", JE.string model.newMessage) ])
      in
        ( { model
          | newMessage = ""
          }
        , Phoenix.SocketOld.push "rooms:lobby" "new:msg" payload model.phxSocket
        )

    SetNewMessage str ->
      ( { model | newMessage = str }
      , Cmd.none
      )

    ReceivePhxMessage chatMessage ->
      ( { model | messages = (chatMessage.user ++ ": " ++ chatMessage.body) :: model.messages }
      , Cmd.none
      )

    JoinChannel ->
      let
        (phxSocket, phxCmd) = Phoenix.SocketOld.join "rooms:123" userParams model.phxSocket
      in
        ({ model | phxSocket = phxSocket }
        , phxCmd
        )

    LeaveChannel ->
      let
        (phxSocket, phxCmd) = Phoenix.SocketOld.leave "rooms:lobby" model.phxSocket
      in
        ({ model | phxSocket = phxSocket }
        , phxCmd
        )      

    NoOp ->
      ( model, Cmd.none )


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Messages:" ]
    , div
        []
        [ button [ onClick JoinChannel ] [ text "Join channel" ]
        , button [ onClick LeaveChannel ] [ text "Leave channel" ]
        ]
    , br [] []
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
