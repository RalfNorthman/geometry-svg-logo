module Main exposing (Model, Msg(..), init, main, myPattern, update, view)

import Angle exposing (Angle)
import Browser
import Color
import Direction2d
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FlatColors.IndianPalette as Palette
import Frame2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Pixels exposing (Pixels, inPixels, pixels)
import Point2d
import Point3d
import Polygon2d
import Quantity exposing (Quantity)
import Rectangle2d
import Svg exposing (Svg)
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Types exposing (..)
import Vector2d


type alias Model =
    { lengthA : Quantity Float Pixels
    , lengthB : Quantity Float Pixels
    }


init : Model
init =
    { lengthA = pixels 40.0
    , lengthB = pixels 14.0
    }


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


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column
            [ Element.spacing 20
            , Element.padding 10
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
            , Element.el [] (Element.html <| myPattern model)
            ]


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
                , Border.rounded 2
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


myPattern : Model -> Html Msg
myPattern model =
    let
        sceneWidth =
            500

        square side color =
            Svg.rectangle2d
                [ TypedSvg.Attributes.fill <| Paint color
                , TypedSvg.Attributes.stroke <| Paint Color.black
                , TypedSvg.Attributes.InPx.strokeWidth 1
                ]
            <|
                Rectangle2d.from Point2d.origin (Point2d.xy side side)

        -- The names of these -ish-vectors only makes sense when lengthA is significantly bigger than lengthB
        rightish =
            Vector2d.xy model.lengthA model.lengthB

        leftish =
            rightish |> Vector2d.reverse

        upish =
            Vector2d.perpendicularTo rightish

        squareA =
            square model.lengthA colorA

        squareB =
            square model.lengthB colorB
                |> Svg.translateBy downB

        downB =
            Vector2d.withLength model.lengthB Direction2d.negativeY

        neededSquares =
            TypedSvg.g []
                [ squareA
                , squareA
                    |> Svg.translateBy leftish
                , squareA
                    |> Svg.translateBy upish
                , squareB
                    |> Svg.translateBy upish
                , squareB
                    |> Svg.translateBy upish
                    |> Svg.translateBy rightish
                , squareB
                    |> Svg.translateBy (Vector2d.twice upish)
                    |> Svg.translateBy rightish
                ]

        negativeAngle =
            Angle.atan2
                (Quantity.negate model.lengthB)
                model.lengthA

        hypotenuse =
            Quantity.sqrt <|
                Quantity.plus
                    (Quantity.squared model.lengthA)
                    (Quantity.squared model.lengthB)

        defs =
            TypedSvg.defs []
                [ TypedSvg.pattern
                    [ TypedSvg.Attributes.id "Pattern"
                    , TypedSvg.Attributes.InPx.x 0
                    , TypedSvg.Attributes.InPx.y 0
                    , TypedSvg.Attributes.InPx.width <| inPixels hypotenuse
                    , TypedSvg.Attributes.InPx.height <| inPixels hypotenuse
                    , TypedSvg.Attributes.patternUnits
                        CoordinateSystemUserSpaceOnUse
                    ]
                    [ neededSquares
                        |> Svg.rotateAround Point2d.origin negativeAngle
                    ]
                ]

        tiling =
            Svg.rectangle2d
                [ TypedSvg.Attributes.fill <| Reference "Pattern" ]
            <|
                Rectangle2d.from Point2d.origin
                    (Point2d.pixels sceneWidth sceneWidth)

        elements =
            TypedSvg.g []
                [ defs
                , tiling
                , neededSquares
                    |> Svg.translateBy rightish
                    |> Svg.translateBy rightish
                    |> Svg.translateBy rightish
                    |> Svg.translateBy rightish
                    |> Svg.translateBy rightish
                    |> Svg.translateBy upish
                ]

        topLeftFrame =
            Frame2d.atPoint (Point2d.pixels 0 sceneWidth)
                |> Frame2d.reverseY

        scene =
            Svg.relativeTo topLeftFrame elements
    in
    Svg.svg
        [ TypedSvg.Attributes.InPx.width sceneWidth
        , TypedSvg.Attributes.InPx.height sceneWidth
        ]
        [ scene ]


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , update = \message model -> ( update message model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        }
