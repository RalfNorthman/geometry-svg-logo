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
                { label = "LengthA:"
                , input = model.lengthA
                , msg = LengthA
                , min = 6
                , max = 60
                }
            , mySlider
                { label = "LengthB: "
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

        rightA =
            Vector2d.withLength model.lengthA Direction2d.x

        rightB =
            Vector2d.withLength model.lengthB Direction2d.x

        upA =
            Vector2d.perpendicularTo rightA

        upB =
            Vector2d.withLength model.lengthB Direction2d.y

        leftB =
            Vector2d.perpendicularTo upB

        squareA =
            square model.lengthA colorA

        squareB =
            square model.lengthB colorB

        primitiveUnit =
            TypedSvg.g []
                [ squareA
                , squareB
                    |> Svg.translateBy rightA
                ]

        defs =
            let
                side =
                    inPixels model.lengthA
                        + inPixels model.lengthB
            in
            TypedSvg.defs []
                [ TypedSvg.pattern
                    [ TypedSvg.Attributes.id "Pattern"
                    , TypedSvg.Attributes.InPx.x 0
                    , TypedSvg.Attributes.InPx.y 0
                    , TypedSvg.Attributes.InPx.width side
                    , TypedSvg.Attributes.InPx.height side
                    , TypedSvg.Attributes.patternUnits
                        CoordinateSystemUserSpaceOnUse
                    ]
                    [ primitiveUnit ]
                ]

        tile =
            Svg.rectangle2d
                [ TypedSvg.Attributes.fill <| Reference "Pattern" ]
            <|
                Rectangle2d.from Point2d.origin
                    (Point2d.pixels sceneWidth sceneWidth)

        elements =
            TypedSvg.g []
                [ defs
                , tile
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
