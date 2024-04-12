module Solitair exposing (Model, Msg, standard, update, view)

import Html exposing (Html)
import Set exposing (Set)
import Solitair.Board as Board


type Model
    = Solitair
        { board : Board.Model
        , pegs : Set Board.Position
        , selected : Maybe Board.Position
        }


standard : Model
standard =
    Solitair
        { board = Board.standard
        , pegs =
            [ [ ( -1, 3 ), ( 0, 3 ), ( 1, 3 ) ]
            , [ ( -1, 2 ), ( 0, 2 ), ( 1, 2 ) ]
            , [ ( -3, 1 ), ( -2, 1 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ), ( 2, 1 ), ( 3, 1 ) ]
            , [ ( -3, 0 ), ( -2, 0 ), ( -1, 0 ), ( 1, 0 ), ( 2, 0 ), ( 3, 0 ) ]
            , [ ( -3, -1 ), ( -2, -1 ), ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( 2, -1 ), ( 3, -1 ) ]
            , [ ( -1, -2 ), ( 0, -2 ), ( 1, -2 ) ]
            , [ ( -1, -3 ), ( 0, -3 ), ( 1, -3 ) ]
            ]
                |> List.concatMap (List.map identity)
                |> Set.fromList
        , selected = Nothing
        }


view : Model -> Html Msg
view (Solitair { board, pegs }) =
    Html.div []
        [ Html.h2 [] [ Html.text "Classic" ]
        , Board.view { selectPeg = Select, deselectPeg = Deselect } pegs board
        ]


type Msg
    = Select Board.Position
    | Deselect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Solitair model) =
    case msg of
        Select position ->
            ( Solitair { model | selected = Just position }, Cmd.none )

        Deselect ->
            ( Solitair { model | selected = Nothing }, Cmd.none )
