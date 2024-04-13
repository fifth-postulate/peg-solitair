module Solitair exposing (Model, Msg, standard, update, view)

import Html exposing (Html)
import Html.Events as Event
import Set exposing (Set)
import Solitair.Board as Board


type Model
    = Solitair
        { board : Board.Model
        , pegs : Set Board.Position
        , selected : Maybe Board.Position
        , history : List Move
        }


type alias Move =
    ( Board.Position, Board.Position, Board.Position )


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
        , history = []
        }


view : Model -> Html Msg
view ((Solitair { board, pegs, selected }) as model) =
    let
        moveable : Move -> Bool
        moveable ( x, y, z ) =
            Set.member x pegs && Set.member y pegs && not (Set.member z pegs)

        candidates : List Move
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
        , viewUndo model
        ]


viewUndo : Model -> Html Msg
viewUndo (Solitair { history }) =
    let
        elements =
            if not (List.isEmpty history) then
                [ Html.button [ Event.onClick Undo ] [ Html.text "←" ] ]

            else
                []
    in
    Html.div [] elements


type Msg
    = Select Board.Position
    | Deselect
    | Move Move
    | Undo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((Solitair m) as model) =
    case msg of
        Select position ->
            ( Solitair { m | selected = Just position }, Cmd.none )

        Deselect ->
            ( Solitair { m | selected = Nothing }, Cmd.none )

        Move move ->
            ( do move model, Cmd.none )

        Undo ->
            ( undo model, Cmd.none )


do : Move -> Model -> Model
do (( p, q, r ) as move) ((Solitair m) as model) =
    let
        ( pegs, history ) =
            if legal move model then
                ( m.pegs
                    |> Set.remove p
                    |> Set.remove q
                    |> Set.insert r
                , move :: m.history
                )

            else
                ( m.pegs, m.history )
    in
    Solitair { m | selected = Nothing, pegs = pegs, history = history }


legal : Move -> Model -> Bool
legal ( p, q, r ) (Solitair { pegs }) =
    Set.member p pegs && Set.member q pegs && not (Set.member r pegs)


undo : Model -> Model
undo (Solitair m) =
    let
        ( pegs, history ) =
            case m.history of
                ( p, q, r ) :: rest ->
                    ( m.pegs
                        |> Set.insert p
                        |> Set.insert q
                        |> Set.remove r
                    , rest
                    )

                [] ->
                    ( m.pegs, m.history )
    in
    Solitair { m | selected = Nothing, pegs = pegs, history = history }
