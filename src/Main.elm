module Main exposing (main)

import Platform


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Platform.Program () Model Msg
main =
    Platform.worker
        { init = \() -> init
        , update = update
        , subscriptions = subscriptions
        }
