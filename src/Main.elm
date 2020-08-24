module Main exposing (Model, Msg(..), init, main, myPattern, update, view)

import Angle exposing (Angle)
import Browser
import Color
import Direction2d
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import FlatColors.IndianPalette as Palette
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Pixels exposing (Pixels, inPixels, pixels)
import Point2d
import Polygon2d exposing (Polygon2d)
import Quantity exposing (Quantity)
import Rectangle2d
import Svg exposing (Svg)
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Types exposing (CoordinateSystem(..), Paint(..))
import Vector2d exposing (Vector2d)



---- MODEL ----


type alias Model =
    { lengthA : Quantity Float Pixels
    , lengthB : Quantity Float Pixels
    }


init : Model
init =
    { lengthA = pixels 40.0
    , lengthB = pixels 14.0
    }



---- UPDATE ----


type Msg
    = LengthA Float
    | LengthB Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        LengthA input ->
            { model
                | lengthA = pixels input
            }

        LengthB input ->
            { model
                | lengthB = pixels input
            }



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column
            [ Element.spacing 30
            , Element.padding 30
            , Element.centerX
            ]
            [ mySlider
                { label = "Side length of green square:"
                , input = model.lengthA
                , msg = LengthA
                , min = 6
                , max = 60
                }
            , mySlider
                { label = "Side length of purple square:"
                , input = model.lengthB
                , msg = LengthB
                , min = 6
                , max = 60
                }
            , Element.el [] <| Element.html <| myPattern model
            ]



---- Colors ----


grey : Element.Color
grey =
    Element.rgb 0.5 0.5 0.5


colorA : Color.Color
colorA =
    let
        { red, green, blue } =
            Palette.oasisStreamRgb
    in
    Color.rgb255 red green blue


colorB : Color.Color
colorB =
    let
        { red, green, blue } =
            Palette.brightUbeRgb
    in
    Color.rgb255 red green blue



---- Slider ----


type alias MySliderInput =
    { label : String
    , input : Quantity Float Pixels
    , msg : Float -> Msg
    , min : Float
    , max : Float
    }


mySlider : MySliderInput -> Element Msg
mySlider input =
    Input.slider
        [ Element.behindContent <|
            Element.el
                [ Element.width Element.fill
                , Element.height <| Element.px 2
                , Element.centerY
                , Background.color grey
                ]
                Element.none
        ]
        { onChange = input.msg
        , label = Input.labelAbove [ Font.color grey ] <| Element.text input.label
        , min = input.min
        , max = input.max
        , value = inPixels input.input
        , thumb = Input.defaultThumb
        , step = Nothing
        }



---- Pythagorean tiling ----


type YUpCoordinates
    = YUpCoordinates


type YDownCoordinates
    = YDownCoordinates


type TopLeftCorner
    = TopLeftCorner


polygonMaker firstPoint vectorList =
    let
        makePoint head angle =
            []

        points vecList angle =
            case vecList of
                [] ->
                    []

                head_ :: tail ->
                    firstPoint :: makePoint head_ angle :: points tail angle
    in
    Polygon2d.singleLoop <| points vectorList (Angle.radians 0)


myPattern : Model -> Html Msg
myPattern model =
    let
        sceneWidth : Quantity number Pixels
        sceneWidth =
            pixels 500

        myPolygon : Quantity Float Pixels -> Color.Color -> Svg msg
        myPolygon color =
            Svg.polygon2d
                [ TypedSvg.Attributes.fill <| Paint color
                , TypedSvg.Attributes.stroke <| Paint Color.black
                , TypedSvg.Attributes.InPx.strokeWidth 1
                ]
            <|
                polygonMaker
                    Point2d.origin
                    []

        rightish : Vector2d Pixels YUpCoordinates
        rightish =
            Vector2d.xy model.lengthA model.lengthB

        leftish : Vector2d Pixels YUpCoordinates
        leftish =
            rightish |> Vector2d.reverse

        upish : Vector2d Pixels YUpCoordinates
        upish =
            Vector2d.perpendicularTo rightish

        polygon : Svg msg
        polygon =
            myPolygon colorA

        downB : Vector2d Pixels YUpCoordinates
        downB =
            Vector2d.withLength model.lengthB Direction2d.negativeY

        hypotenuse : Quantity a Pixels -> Quantity a Pixels -> Quantity a Pixels
        hypotenuse legA legB =
            Quantity.sqrt <|
                Quantity.plus
                    (Quantity.squared legA)
                    (Quantity.squared legB)

        defs : Svg msg
        defs =
            TypedSvg.defs []
                [ TypedSvg.pattern
                    [ TypedSvg.Attributes.id "Pattern"
                    , TypedSvg.Attributes.InPx.x 0
                    , TypedSvg.Attributes.InPx.y 0
                    , TypedSvg.Attributes.InPx.width <| inPixels 100
                    , TypedSvg.Attributes.InPx.height <| inPixels 100
                    , TypedSvg.Attributes.patternUnits
                        CoordinateSystemUserSpaceOnUse
                    ]
                    [ polygon ]
                ]

        tiling : Svg msg
        tiling =
            Svg.rectangle2d
                [ TypedSvg.Attributes.fill <| Reference "Pattern" ]
            <|
                Rectangle2d.from Point2d.origin
                    (Point2d.xy sceneWidth sceneWidth)

        elements : Svg msg
        elements =
            TypedSvg.g [] [ defs, tiling ]

        topLeftFrame : Frame2d Pixels YDownCoordinates { defines : TopLeftCorner }
        topLeftFrame =
            Frame2d.atPoint (Point2d.xy (pixels 0) sceneWidth)
                |> Frame2d.reverseY

        scene : Svg msg
        scene =
            elements |> Svg.relativeTo topLeftFrame
    in
    Svg.svg
        [ TypedSvg.Attributes.InPx.width (inPixels sceneWidth)
        , TypedSvg.Attributes.InPx.height (inPixels sceneWidth)
        ]
        [ scene ]



---- Main ----


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , update = \message model -> ( update message model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        }
