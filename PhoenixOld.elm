module PhoenixOld exposing (Socket)

import WebSocket
import Platform.Sub
import Dict exposing (Dict)
import Json.Decode as JD exposing ((:=))

type alias Socket msg =
  { path : String
  , events : Dict (Channel, Event) (JD.Decoder msg)
  }

type alias Channel = String
type alias Event = String

connect : String -> Socket msg
connect path =
  Socket path Dict.empty

join : Channel -> Socket msg -> Cmd msg
join channel socket =
  WebSocket.send socket.path "asdf"

on : Event -> Channel -> JD.Decoder msg -> Socket msg -> Socket msg
on event channel decoder socket =
  { socket | events = Dict.insert (channel, event) decoder socket.events }
  

listen : msg -> Socket msg -> Sub msg
listen defaultMsg socket =
  WebSocket.listen socket.path (handleEvent defaultMsg socket)

type alias Packet =
  { topic : String
  , ref : String
  , payload : String
  , event : String
  }

phoenixMessageDecoder : JD.Decoder Packet
phoenixMessageDecoder =
  JD.object4 Packet
    ("topic" := JD.string)
    ("ref" := JD.string)
    ("payload" := JD.string)
    ("event" := JD.string)


handleEvent : msg -> Socket msg -> String -> msg
handleEvent default {events} string =
  case JD.decodeString phoenixMessageDecoder string of
    Ok val ->
      default
    Err errorMessage ->
      default
