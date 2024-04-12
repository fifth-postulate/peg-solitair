module Solitair.Board exposing (Model, Position, standard, view)

import Set exposing (Set)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as Attribute


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
    { peg : Circular {}
    , hole : Circular {}
    }


type alias Circular e =
    { e | radius : String, stroke : String, fill : String }


default : Configuration
default =
    { peg = { radius = "0.3", stroke = "black", fill = "black" }
    , hole = { radius = "0.1", stroke = "gray", fill = "white" }
    }


view : Set Position -> Model -> Svg msg
view =
    viewTemplate default


viewTemplate : Configuration -> Set Position -> Model -> Svg msg
viewTemplate config pegs (Board { kind, positions }) =
    let
        locate =
            location kind

        viewBox =
            positions
                |> boundingBox
                |> Maybe.map (Tuple.mapBoth locate locate)
                |> Maybe.withDefault ( ( 0, 0 ), ( 1, 1 ) )
                |> toViewBox
    in
    Svg.svg [ viewBox, Attribute.width "60vmin", Attribute.height "60vmin" ]
        [ positions
            |> Set.map locate
            |> Set.toList
            |> List.map (viewHole config.hole)
            |> Svg.g [ Attribute.strokeWidth "0.01", Attribute.stroke config.hole.stroke, Attribute.fill config.hole.fill ]
        , pegs
            |> Set.map locate
            |> Set.toList
            |> List.map (viewPeg config.peg)
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


viewHole : Circular {} -> Location -> Svg msg
viewHole config ( x, y ) =
    Svg.circle [ Attribute.r config.radius, Attribute.cx <| String.fromFloat x, Attribute.cy <| String.fromFloat y ] []


viewPeg : Circular {} -> Location -> Svg msg
viewPeg config ( x, y ) =
    Svg.circle [ Attribute.r config.radius, Attribute.cx <| String.fromFloat x, Attribute.cy <| String.fromFloat y ] []
