# elm-phoenix-socket

This library is a pure Elm interpretation of the Phoenix.Socket library
that comes bundled with the Phoenix web framework. It aims to abstract away
the more tedious bits of communicating with Phoenix, such as joining channels,
leaving channels, registering event handlers, and handling errors.

## Setup

Phoenix connections are stateful. The Socket module will manage all of this for you,
but you need to add some boilerplate to your project to wire everything up.

1. Import all three `Phoenix` modules

    ```elm
    import Phoenix.Socket
    import Phoenix.Channel
    import Phoenix.Push
    ```

2. Add a socket to your model

    ```elm
    type alias Model =
      { phxSocket : Phoenix.Socket.Socket Msg
      }
    ```

3. Initialize the socket. The default path for Phoenix in development is `"ws://localhost:4000/socket/websocket"`.

    ```elm
    init =
      { phxSocket = Phoenix.Socket.init "PATH_TO_SERVER"
      }
    ```

4. Add a PhoenixMsg tag to your Msg type

    ```elm
    type Msg
      = UpdateSomething
      | DoSomethingElse
      | PhoenixMsg (Phoenix.Socket.Msg Msg)

    ```

5. Add the following to your update function

    ```elm
    PhoenixMsg msg ->
      let
        ( phxSocket, phxCmd ) = Phoenix.Socket.update msg model.phxSocket
      in
        ( { model | phxSocket = phxSocket }
        , Cmd.map PhoenixMsg phxCmd
        )
    ```

6. Listen for messages

    ```elm
    subscriptions : Model -> Sub Msg
    subscriptions model =
      Phoenix.Socket.listen model.phxSocket PhoenixMsg
    ```

Take a look at examples/Chat.elm if you want to see an example.

## Contributing

Pull requests and issues are greatly appreciated! If you think there's a better way
to implement this library, I'd love to hear your feedback. I basically tried to model
this library after the official javascript one, but that may not be the best approach.