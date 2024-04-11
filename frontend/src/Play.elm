module Play exposing (main)

import Browser exposing (Document)
import Html


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Blank


init : () -> ( Model, Cmd Msg )
init _ =
    ( Blank, Cmd.none )


view : Model -> Document Msg
view _ =
    { title = "Peg Solitair"
    , body = [ Html.text "Hello, World!" ]
    }


type Msg
    = Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
