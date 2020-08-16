module Main exposing (Model, Msg(..), init, inputField, logo, main, update, view)

import Angle exposing (Angle)
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Frame2d
import Frame3d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import Html.Events as Events
import Point2d
import Point3d
import Polygon2d
import Quantity
import Svg exposing (Svg)
import Svg.Attributes


type alias Model =
    { height : Float
    , xOffset : Float
    , yOffset : Float
    , zOffset : Float
    , azimuth : Angle
    , elevation : Angle
    }


init : Model
init =
    { height = 0.8
    , xOffset = 0.6
    , yOffset = 0.6
    , zOffset = 0.6
    , azimuth = Angle.degrees 65
    , elevation = Angle.degrees 20
    }


type Msg
    = HeightInput Float
    | XOffsetInput Float
    | YOffsetInput Float
    | ZOffsetInput Float
    | AzimuthInput Float
    | ElevationInput Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        HeightInput input ->
            { model
                | height = input
            }

        XOffsetInput input ->
            { model
                | xOffset = input
            }

        YOffsetInput input ->
            { model
                | yOffset = input
            }

        ZOffsetInput input ->
            { model
                | zOffset = input
            }

        AzimuthInput input ->
            { model
                | azimuth = Angle.degrees input
            }

        ElevationInput input ->
            { model
                | elevation = Angle.degrees input
            }


view : Model -> Html Msg
view model =
    layout [] <|
        column
            [ spacing 20, padding 10, centerX ]
            [ mySlider
                { label = "Height:"
                , input = model.height
                , msg = HeightInput
                , min = 0
                , max = 1
                }
            , mySlider
                { label = "X Offset: "
                , input = model.xOffset
                , msg = XOffsetInput
                , min = 0
                , max = 1
                }
            , mySlider
                { label = "Y Offset: "
                , input = model.yOffset
                , msg = YOffsetInput
                , min = 0
                , max = 1
                }
            , mySlider
                { label = "Z Offset: "
                , input = model.zOffset
                , msg = ZOffsetInput
                , min = 0
                , max = 1
                }
            , mySlider
                { label = "Azimuth: "
                , input = Angle.inDegrees model.azimuth
                , msg = AzimuthInput
                , min = 0
                , max = 90
                }
            , mySlider
                { label = "Elevation: "
                , input = Angle.inDegrees model.elevation
                , msg = ElevationInput
                , min = 0
                , max = 90
                }
            , html <| logo model
            ]


grey : Color
grey =
    rgb 0.5 0.5 0.5


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
        [ behindContent <|
            el
                [ width fill
                , height <| px 2
                , centerY
                , Background.color grey
                , Border.rounded 2
                ]
                none
        ]
        { onChange = input.msg
        , label = Input.labelAbove [ Font.color grey ] <| text input.label
        , min = input.min
        , max = input.max
        , value = input.input
        , thumb = Input.defaultThumb
        , step = Nothing
        }


inputField : String -> String -> (String -> Msg) -> Html Msg
inputField label value msg =
    Html.div []
        [ Html.label [] [ Html.text label ]
        , Html.input
            [ Html.Attributes.type_ "text"
            , Html.Attributes.value value
            , Events.onInput msg
            ]
            []
        ]


logo : Model -> Html Msg
logo model =
    let
        p1 =
            Point3d.unitless 1 0 0

        p2 =
            Point3d.unitless 1 1 0

        p3 =
            Point3d.unitless 0 1 0

        p4 =
            Point3d.unitless 0 1 model.height

        p5 =
            Point3d.unitless 0 0 model.height

        p6 =
            Point3d.unitless 1 0 model.height

        p7 =
            Point3d.unitless 1 (1 - model.yOffset) model.height

        p8 =
            Point3d.unitless 1 1 (model.height - model.zOffset)

        p9 =
            Point3d.unitless (1 - model.xOffset) 1 model.height

        eyePoint =
            Point3d.unitless 0.5 0.5 (model.height / 2)

        viewFrame =
            Frame3d.atPoint eyePoint
                |> Frame3d.rotateAroundOwn Frame3d.zAxis model.azimuth
                |> Frame3d.rotateAroundOwn Frame3d.yAxis (Quantity.negate model.elevation)

        to2d =
            Point3d.projectInto (Frame3d.yzSketchPlane viewFrame)

        leftPolygon =
            Polygon2d.singleLoop (List.map to2d [ p1, p2, p8, p7, p6 ])

        rightPolygon =
            Polygon2d.singleLoop (List.map to2d [ p2, p3, p4, p9, p8 ])

        topPolygon =
            Polygon2d.singleLoop (List.map to2d [ p6, p7, p9, p4, p5 ])

        trianglePolygon =
            Polygon2d.singleLoop (List.map to2d [ p7, p8, p9 ])

        orange =
            "rgb(240, 173, 0)"

        green =
            "rgb(127, 209, 59)"

        lightBlue =
            "rgb(96, 181, 204)"

        darkBlue =
            "rgb(90, 99, 120)"

        mask id polygon =
            let
                attributes =
                    [ Svg.Attributes.fill "white"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeWidth "0.03"
                    ]
            in
            Svg.mask [ Svg.Attributes.id id ]
                [ Svg.polygon2d attributes polygon ]

        face color clipPathId polygon =
            let
                attributes =
                    [ Svg.Attributes.fill color
                    , Svg.Attributes.mask ("url(#" ++ clipPathId ++ ")")
                    ]
            in
            Svg.polygon2d attributes polygon

        defs =
            Svg.defs []
                [ mask "leftOutline" leftPolygon
                , mask "rightOutline" rightPolygon
                , mask "topOutline" topPolygon
                , mask "triangleOutline" trianglePolygon
                ]

        leftFace =
            face orange "leftOutline" leftPolygon

        rightFace =
            face lightBlue "rightOutline" rightPolygon

        topFace =
            face green "topOutline" topPolygon

        triangleFace =
            face darkBlue "triangleOutline" trianglePolygon

        elements =
            Svg.g [] [ defs, leftFace, rightFace, topFace, triangleFace ]

        topLeftFrame =
            Frame2d.atPoint (Point2d.pixels -250 250)
                |> Frame2d.reverseY

        scene =
            Svg.relativeTo topLeftFrame
                (Svg.scaleAbout Point2d.origin 200 elements)
    in
    Svg.svg [ Svg.Attributes.width "500", Svg.Attributes.height "500" ]
        [ scene ]


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , update = \message model -> ( update message model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        }
