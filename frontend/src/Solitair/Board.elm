module Solitair.Board exposing (Model, Position, moveCandidates, standard, view)

import List exposing (singleton)
import Set exposing (Set)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as Attribute
import Svg.Events as Event


type Model
    = Board
        { kind : Kind
        , positions : Set Position
        }


type Kind
    = Rectangular
    | Hexagonal


type alias Position =
    ( Int, Int )


type alias Location =
    ( Float, Float )


type alias Translation =
    ( Int, Int )


translate : Translation -> Position -> Position
translate ( dx, dy ) ( x, y ) =
    ( x + dx, y + dy )


scale : Int -> Translation -> Translation
scale k ( dx, dy ) =
    ( k * dx, k * dy )


movesFrom : Kind -> Position -> List ( Position, Position, Position )
movesFrom kind position =
    let
        directions : List Translation
        directions =
            case kind of
                Rectangular ->
                    [ ( 1, 0 ), ( 0, 1 ), ( -1, 0 ), ( 0, -1 ) ]

                Hexagonal ->
                    [ ( 1, 0 ), ( 0, 1 ), ( -1, 1 ), ( -1, 0 ), ( 0, -1 ), ( 1, -1 ) ]

        candidate : Translation -> ( Position, Position, Position )
        candidate translation =
            ( translate (scale 0 translation) position
            , translate (scale 1 translation) position
            , translate (scale 2 translation) position
            )
    in
    directions
        |> List.map candidate


moveCandidates : Position -> Model -> List ( Position, Position, Position )
moveCandidates position (Board { kind, positions }) =
    let
        allInBoard : ( Position, Position, Position ) -> Bool
        allInBoard ( x, y, z ) =
            [ x, y, z ]
                |> List.all (\u -> Set.member u positions)
    in
    position
        |> movesFrom kind
        |> List.filter allInBoard


standard : Model
standard =
    Board
        { kind = Rectangular
        , positions =
            [ [ ( -1, 3 ), ( 0, 3 ), ( 1, 3 ) ]
            , [ ( -1, 2 ), ( 0, 2 ), ( 1, 2 ) ]
            , [ ( -3, 1 ), ( -2, 1 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ), ( 2, 1 ), ( 3, 1 ) ]
            , [ ( -3, 0 ), ( -2, 0 ), ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 3, 0 ) ]
            , [ ( -3, -1 ), ( -2, -1 ), ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( 2, -1 ), ( 3, -1 ) ]
            , [ ( -1, -2 ), ( 0, -2 ), ( 1, -2 ) ]
            , [ ( -1, -3 ), ( 0, -3 ), ( 1, -3 ) ]
            ]
                |> List.concatMap (List.map identity)
                |> Set.fromList
        }


boundingBox : Set Position -> Maybe ( Position, Position )
boundingBox positions =
    let
        envelop : Position -> ( Position, Position ) -> ( Position, Position )
        envelop ( x, y ) ( ( llx, lly ), ( urx, ury ) ) =
            ( ( min x llx, min y lly ), ( max x urx, max y ury ) )
    in
    case Set.toList positions of
        x :: xs ->
            List.foldl envelop ( x, x ) xs
                |> Just

        [] ->
            Nothing


location : Kind -> Position -> Location
location kind ( x, y ) =
    let
        ds =
            sqrt 3 / 2.0

        lx =
            toFloat x

        ly =
            toFloat y
    in
    case kind of
        Rectangular ->
            ( lx, ly )

        Hexagonal ->
            ( lx + ly / 2.0, ds * ly )


type alias Configuration =
    { peg : Circular (Selectable (Circular {}))
    , hole : Circular (Selectable (Circular {}))
    }


type alias Circular e =
    { e | radius : String, stroke : String, fill : String }


type alias Selectable e =
    { selected : e
    }


default : Configuration
default =
    { peg =
        { radius = "0.3"
        , stroke = "black"
        , fill = "black"
        , selected = { radius = "0.3", stroke = "blue", fill = "blue" }
        }
    , hole =
        { radius = "0.2"
        , stroke = "gray"
        , fill = "white"
        , selected = { radius = "0.2", stroke = "gray", fill = "green" }
        }
    }


type alias Messages msg =
    { selectPeg : Position -> msg
    , deselectPeg : msg
    , move : ( Position, Position, Position ) -> msg
    }


view : Messages msg -> Maybe Position -> List ( Position, Position, Position ) -> Set Position -> Model -> Svg msg
view =
    viewTemplate default


viewTemplate : Configuration -> Messages msg -> Maybe Position -> List ( Position, Position, Position ) -> Set Position -> Model -> Svg msg
viewTemplate config messages selectedPeg candidates pegs (Board { kind, positions }) =
    let
        locate =
            location kind

        viewBox =
            positions
                |> boundingBox
                |> Maybe.map (Tuple.mapBoth locate locate)
                |> Maybe.withDefault ( ( 0, 0 ), ( 1, 1 ) )
                |> toViewBox

        viewAHole : ( Position, Location ) -> Svg msg
        viewAHole ( p, l ) =
            let
                move =
                    candidates
                        |> List.filter (\( _, _, q ) -> p == q)
            in
            case move of
                m :: _ ->
                    viewHole (Just <| messages.move m) config.hole.selected l

                [] ->
                    viewHole Nothing config.hole l

        viewAPeg : ( Position, Location ) -> Svg msg
        viewAPeg ( p, l ) =
            case selectedPeg of
                Just q ->
                    if p == q then
                        viewPeg messages.deselectPeg config.peg.selected l

                    else
                        viewPeg (messages.selectPeg p) config.peg l

                _ ->
                    viewPeg (messages.selectPeg p) config.peg l
    in
    Svg.svg [ viewBox, Attribute.width "60vmin", Attribute.height "60vmin" ]
        [ positions
            |> Set.map (\p -> ( p, locate p ))
            |> Set.toList
            |> List.map viewAHole
            |> Svg.g [ Attribute.strokeWidth "0.01", Attribute.stroke config.hole.stroke, Attribute.fill config.hole.fill ]
        , pegs
            |> Set.map (\p -> ( p, locate p ))
            |> Set.toList
            |> List.map viewAPeg
            |> Svg.g [ Attribute.strokeWidth "0.01", Attribute.stroke config.peg.stroke, Attribute.fill config.peg.fill ]
        ]


toViewBox : ( Location, Location ) -> Attribute msg
toViewBox ( ( llx, lly ), ( urx, ury ) ) =
    let
        margin =
            1
    in
    [ llx - margin, lly - margin, 2 * margin + urx - llx, 2 * margin + ury - lly ]
        |> List.map String.fromFloat
        |> String.join " "
        |> Attribute.viewBox


viewHole : Maybe msg -> Circular e -> Location -> Svg msg
viewHole msg config ( x, y ) =
    let
        attribute =
            msg
                |> Maybe.map Event.onClick
                |> Maybe.map singleton
                |> Maybe.withDefault []
    in
    Svg.circle
        (attribute
            ++ [ Attribute.r config.radius
               , Attribute.cx <| String.fromFloat x
               , Attribute.cy <| String.fromFloat y
               , Attribute.stroke config.stroke
               , Attribute.fill config.fill
               ]
        )
        []


viewPeg : msg -> Circular e -> Location -> Svg msg
viewPeg message config ( x, y ) =
    Svg.circle
        [ Event.onClick message
        , Attribute.r config.radius
        , Attribute.cx <| String.fromFloat x
        , Attribute.cy <| String.fromFloat y
        , Attribute.stroke config.stroke
        , Attribute.fill config.fill
        ]
        []
