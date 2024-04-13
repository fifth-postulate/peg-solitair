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
view (Solitair { board, pegs, selected }) =
    let
        moveable : ( Board.Position, Board.Position, Board.Position ) -> Bool
        moveable ( x, y, z ) =
            Set.member x pegs && Set.member y pegs && not (Set.member z pegs)

        candidates : List ( Board.Position, Board.Position, Board.Position )
        candidates =
            selected
                |> Maybe.map (\p -> Board.moveCandidates p board)
                |> Maybe.map (List.filter moveable)
                |> Maybe.withDefault []
    in
    Html.div []
        [ Html.h2 [] [ Html.text "Classic" ]
        , Board.view
            { selectPeg = Select
            , deselectPeg = Deselect
            , move = Move
            }
            selected
            candidates
            pegs
            board
        ]


type Msg
    = Select Board.Position
    | Deselect
    | Move ( Board.Position, Board.Position, Board.Position )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((Solitair m) as model) =
    case msg of
        Select position ->
            ( Solitair { m | selected = Just position }, Cmd.none )

        Deselect ->
            ( Solitair { m | selected = Nothing }, Cmd.none )

        Move (( p, q, r ) as move) ->
            let
                pegs =
                    if legal move model then
                        m.pegs
                            |> Set.remove p
                            |> Set.remove q
                            |> Set.insert r

                    else
                        m.pegs
            in
            ( Solitair { m | selected = Nothing, pegs = pegs }, Cmd.none )


legal : ( Board.Position, Board.Position, Board.Position ) -> Model -> Bool
legal ( p, q, r ) (Solitair { pegs }) =
    Set.member p pegs && Set.member q pegs && not (Set.member r pegs)
