module Play exposing (main)

import Browser exposing (Document)
import Html exposing (Html)
import Solitair


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = App
        { current : Solitair.Model
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( App { current = Solitair.standard }, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Peg Solitair"
    , body =
        [ viewHeader
        , viewMain model
        , viewFooter
        ]
    }


viewHeader : Html Msg
viewHeader =
    Html.header []
        [ Html.h1 [] [ Html.text "Peg Solitair" ]
        ]


viewMain : Model -> Html Msg
viewMain (App { current }) =
    Html.main_ []
        [ Html.map CurrentGame <| Solitair.view current
        ]


viewFooter : Html Msg
viewFooter =
    Html.footer []
        [ Html.h6 [] [ Html.text "Made with ❤️" ]
        ]


type Msg
    = CurrentGame Solitair.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (App app) =
    case msg of
        CurrentGame m ->
            let
                ( current, cmd ) =
                    Solitair.update m app.current

                next =
                    App
                        { app | current = current }
            in
            ( next, Cmd.map CurrentGame cmd )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
