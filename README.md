# elm-phoenix-socket

This library is a pure Elm interpretation of the Phoenix.Socket library
that comes bundled with the Phoenix web framework. It aims to abstract away
the more tedious bits of communicating with Phoenix, such as joining channels,
leaving channels, registering event handlers, and handling errors.


## Setup

Phoenix connections are stateful. The Socket module will manage all of this for you,
but you need to add some boilerplate to your project to wire everything up.

1. Add a socket to your model

    ```elm
    type alias Model =
      { phxSocket : Phoenix.Socket.Socket Msg
      }
    ```

2. Initialize the socket

    ```elm
    init =
      { phxSocket = Phoenix.Socket.init "PATH_TO_SERVER"
      }
    ```

3. Add a PhoenixMsg tag to your Msg type

    ```elm
    type Msg
      = UpdateSomething
      | DoSomethingElse
      | PhoenixMsg
    ```

4. Add the following to your update function

    ```elm
    PhoenixMsg msg ->
      let
        ( phxSocket, phxCmd ) = Phoenix.Socket.update msg model.phxSocket
      in
        ( { model | phxSocket = phxSocket }
        , Cmd.map PhoenixMsg phxCmd
        )
    ```

5. Listen for messages

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


## To do:

- If a channel errors out, automatically reconnect with an exponential backoff strategy
- Write tests
