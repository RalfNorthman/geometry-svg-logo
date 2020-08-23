module Main exposing (Model, Msg(..), init, main, myPattern, update, view)

import Angle exposing (Angle)
import Browser
import Color
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FlatColors.IndianPalette as Palette
import Frame2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Pixels
import Point2d
import Point3d
import Polygon2d
import Rectangle2d
import Svg exposing (Svg)
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Types exposing (..)


type alias Model =
    { lengthA : Float
    , lengthB : Float
    }


init : Model
init =
    { lengthA = 40.0
    , lengthB = 14.0
    }


type Msg
    = LengthA Float
    | LengthB Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        LengthA input ->
            { model
                | lengthA = input
            }

        LengthB input ->
            { model
                | lengthB = input
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
            , Element.el
                [ Border.width 1
                , Border.color grey
                ]
                (Element.html <| myPattern model)
            ]


grey : Element.Color
grey =
    Element.rgb 0.5 0.5 0.5


colorA =
    let
        { red, green, blue } =
            Palette.oasisStreamRgb
    in
    Color.rgb255 red green blue


colorB =
    let
        { red, green, blue } =
            Palette.brightUbeRgb
    in
    Color.rgb255 red green blue


type alias MySliderInput =
    { label : String
    , input : Float
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
        , value = input.input
        , thumb = Input.defaultThumb
        , step = Nothing
        }


myPattern : Model -> Html Msg
myPattern model =
    let
        p0 =
            Point2d.origin

        myRect bottomleft side color =
            Svg.rectangle2d
                [ TypedSvg.Attributes.fill <| Paint color
                , TypedSvg.Attributes.stroke <| Paint Color.black
                , TypedSvg.Attributes.InPx.strokeWidth 1
                ]
            <|
                Rectangle2d.from
                    bottomleft
                    (Point2d.pixels
                        (Pixels.inPixels (Point2d.xCoordinate bottomleft) + side)
                        (Pixels.inPixels (Point2d.yCoordinate bottomleft) + side)
                    )

        rectA =
            myRect Point2d.origin model.lengthA colorA

        rectB =
            myRect (Point2d.pixels 0 model.lengthA) model.lengthB colorB

        elements =
            TypedSvg.g [] [ rectA, rectB ]

        topLeftFrame =
            Frame2d.atPoint (Point2d.pixels 0 500)
                |> Frame2d.reverseY

        scene =
            Svg.relativeTo topLeftFrame elements
    in
    Svg.svg
        [ TypedSvg.Attributes.InPx.width 500
        , TypedSvg.Attributes.InPx.height 500
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
