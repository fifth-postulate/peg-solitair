module Solitair exposing (Model, Msg, standard, update, view)

import Html exposing (Html)
import Html.Events as Event
import Set exposing (Set)
import Solitair.Board as Board
import Solver


type Model
    = Solitair
        { name : String
        , board : Board.Model
        , pegs : Set Board.Position
        , selected : Maybe Board.Position
        , history : List Move
        , plan : Maybe (List Move)
        }


type alias Move =
    ( Board.Position, Board.Position, Board.Position )


standard : Model
standard =
    Solitair
        { name = "Classic"
        , board = Board.standard
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
        , plan = Nothing
        }


view : Model -> Html Msg
view ((Solitair { name, board, pegs, selected, plan }) as model) =
    let
        candidates : List Move
        candidates =
            selected
                |> Maybe.map (possibleMovesFor model)
                |> Maybe.withDefault []

        marked =
            plan
                |> Maybe.andThen List.head
    in
    Html.div []
        [ Html.h2 [] [ Html.text name ]
        , viewControl model
        , Board.view
            { selectPeg = Select
            , deselectPeg = Deselect
            , move = Move
            }
            selected
            marked
            candidates
            pegs
            board
        ]


viewControl : Model -> Html Msg
viewControl model =
    Html.div []
        [ viewUndo model
        , viewSolve model
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
    Html.span [] elements


viewSolve : Model -> Html Msg
viewSolve (Solitair { plan }) =
    let
        elements =
            case plan of
                Nothing ->
                    [ Html.button [ Event.onClick Plan ] [ Html.text "solve" ] ]

                Just moves ->
                    []
    in
    Html.span [] elements


type Msg
    = Select Board.Position
    | Deselect
    | Move Move
    | Plan
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

        Plan ->
            ( solve model, Cmd.none )

        Undo ->
            ( undo model, Cmd.none )


do : Move -> Model -> Model
do (( p, q, r ) as move) ((Solitair m) as model) =
    let
        ( pegs, history, plan ) =
            if legal model move then
                if Just move == Maybe.andThen List.head m.plan then
                    ( m.pegs
                        |> Set.remove p
                        |> Set.remove q
                        |> Set.insert r
                    , move :: m.history
                    , m.plan |> Maybe.andThen List.tail
                    )

                else
                    ( m.pegs
                        |> Set.remove p
                        |> Set.remove q
                        |> Set.insert r
                    , move :: m.history
                    , Nothing
                    )

            else
                ( m.pegs, m.history, m.plan )
    in
    Solitair { m | selected = Nothing, pegs = pegs, history = history, plan = plan }


possibleMoves : Model -> List Move
possibleMoves ((Solitair { pegs }) as model) =
    pegs
        |> Set.toList
        |> List.concatMap (possibleMovesFor model)


possibleMovesFor : Model -> Board.Position -> List Move
possibleMovesFor ((Solitair { board }) as model) p =
    board
        |> Board.moveCandidates p
        |> List.filter (legal model)


legal : Model -> Move -> Bool
legal (Solitair { pegs }) ( p, q, r ) =
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


solve : Model -> Model
solve ((Solitair m) as model) =
    let
        plan =
            Solver.solve isSolved possibleMoves do model
    in
    Solitair { m | plan = plan }


isSolved : Model -> Bool
isSolved (Solitair m) =
    1 == Set.size m.pegs
