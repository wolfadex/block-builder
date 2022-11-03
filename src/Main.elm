module Main exposing (main)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Block3d
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Direction3d
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Length
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Rectangle2d
import Scene3d
import Scene3d.Material
import Sphere3d exposing (Sphere3d)
import Viewpoint3d


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { testBlockBot : Ent
    , cameraAzimuth : Angle
    , camera : Camera3d Length.Meters GameCoordinates
    , cursorPnt : Point2d Pixels ScreenCoordinates
    }


type alias Ent =
    { entity : Scene3d.Entity GameCoordinates
    , parts : EntParts
    , dragHandles : List DragHandle
    }


type alias DragHandle =
    { entity : Scene3d.Entity GameCoordinates
    , sphere : Sphere3d Length.Meters GameCoordinates
    , state : DragHandleState
    }


type DragHandleState
    = Default
    | Hover
    | Dragging


type EntParts
    = EntBlock (Scene3d.Material.Uniform GameCoordinates) (Block3d.Block3d Length.Meters GameCoordinates)


type GameCoordinates
    = GameCoordinates Never


type ScreenCoordinates
    = ScreenCoordinates Never


init : () -> ( Model, Cmd Msg )
init () =
    ( { testBlockBot =
            EntBlock
                (Scene3d.Material.nonmetal
                    { baseColor = Color.darkGreen
                    , roughness = 0.7
                    }
                )
                (Block3d.from
                    (Point3d.meters -0.5 -0.5 0)
                    (Point3d.meters 0.5 0.5 1)
                )
                |> partToEnt
      , cameraAzimuth = Angle.degrees 0
      , camera = setCamera (Angle.degrees 0)
      , cursorPnt = Point2d.origin
      }
    , Cmd.none
    )


setCamera azimuth =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.origin
                , azimuth = azimuth
                , elevation = Angle.degrees 35
                , distance = Length.meters 10
                }
        , verticalFieldOfView = Angle.degrees 30
        }


partToEnt : EntParts -> Ent
partToEnt parts =
    case parts of
        EntBlock material shape ->
            { entity = Scene3d.blockWithShadow material shape
            , parts = parts
            , dragHandles =
                let
                    ( length, width, height ) =
                        Block3d.dimensions shape

                    center : Point3d Length.Meters GameCoordinates
                    center =
                        Block3d.centerPoint shape

                    xAxis : Axis3d Length.Meters GameCoordinates
                    xAxis =
                        Block3d.xAxis shape

                    yAxis : Axis3d Length.Meters GameCoordinates
                    yAxis =
                        Block3d.yAxis shape

                    zAxis : Axis3d Length.Meters GameCoordinates
                    zAxis =
                        Block3d.zAxis shape
                in
                [ center
                    |> Point3d.translateIn (Axis3d.direction xAxis) width
                    |> createDragHandleSphere
                , center
                    |> Point3d.translateIn (Axis3d.direction xAxis) (Quantity.negate width)
                    |> createDragHandleSphere
                , center
                    |> Point3d.translateIn (Axis3d.direction yAxis) length
                    |> createDragHandleSphere
                , center
                    |> Point3d.translateIn (Axis3d.direction yAxis) (Quantity.negate length)
                    |> createDragHandleSphere
                , center
                    |> Point3d.translateIn (Axis3d.direction zAxis) height
                    |> createDragHandleSphere
                , center
                    |> Point3d.translateIn (Axis3d.direction zAxis) (Quantity.negate height)
                    |> createDragHandleSphere
                ]
                    |> List.map
                        (\sphere ->
                            { sphere = sphere
                            , entity = Scene3d.sphere (Scene3d.Material.color Color.white) sphere
                            , state = Default
                            }
                        )
            }


createDragHandleSphere center =
    Sphere3d.atPoint center (Length.meters 0.25)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown decodeKeyDown
        ]


type Key
    = ArrowRight
    | ArrowLeft


decodeKeyDown : Json.Decode.Decoder Msg
decodeKeyDown =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\keyStr ->
                case keyStr of
                    "ArrowRight" ->
                        Json.Decode.succeed (KeyDown ArrowRight)

                    "ArrowLeft" ->
                        Json.Decode.succeed (KeyDown ArrowLeft)

                    _ ->
                        Json.Decode.fail ("Unsupported key: " ++ keyStr)
            )


decodeMouseMove : Json.Decode.Decoder Msg
decodeMouseMove =
    Json.Decode.map2 (\x y -> MouseMove (Point2d.pixels x y))
        (Json.Decode.field "offsetX" Json.Decode.float)
        (Json.Decode.field "offsetY" Json.Decode.float)


type Msg
    = NoOp
    | KeyDown Key
    | MouseMove (Point2d Pixels ScreenCoordinates)
    | MouseDown
    | MouseUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        KeyDown key ->
            let
                newAzimuth =
                    model.cameraAzimuth
                        |> Quantity.plus
                            (Angle.degrees
                                (case key of
                                    ArrowRight ->
                                        5

                                    ArrowLeft ->
                                        -5
                                )
                            )
            in
            ( { model
                | cameraAzimuth = newAzimuth
                , camera = setCamera newAzimuth
              }
            , Cmd.none
            )

        MouseMove cursorPoint ->
            let
                testBlockBot : Ent
                testBlockBot =
                    model.testBlockBot

                ray : Axis3d Length.Meters GameCoordinates
                ray =
                    Camera3d.ray
                        model.camera
                        (Rectangle2d.from
                            -- bottom left
                            (Point2d.pixels 0 600)
                            -- top right
                            (Point2d.pixels 800 0)
                        )
                        cursorPoint

                rayStart : Point3d Length.Meters GameCoordinates
                rayStart =
                    Axis3d.originPoint ray
            in
            ( { model
                | testBlockBot =
                    { testBlockBot
                        | dragHandles =
                            List.foldl
                                (\dragHandle ( result, maybeNearest ) ->
                                    case ( Axis3d.intersectionWithSphere dragHandle.sphere ray, dragHandle.state ) of
                                        ( Nothing, Default ) ->
                                            ( dragHandle :: result, maybeNearest )

                                        ( Nothing, _ ) ->
                                            ( setDragHandleState Default dragHandle :: result, maybeNearest )

                                        ( Just ( intersectionPoint, _ ), _ ) ->
                                            let
                                                distance : Quantity Float Length.Meters
                                                distance =
                                                    intersectionPoint
                                                        |> Point3d.distanceFrom rayStart
                                            in
                                            case maybeNearest of
                                                Nothing ->
                                                    ( result
                                                    , Just ( setDragHandleState Hover dragHandle, distance )
                                                    )

                                                Just ( nearest, nearestDist ) ->
                                                    if distance |> Quantity.lessThan nearestDist then
                                                        ( setDragHandleState Default nearest :: result
                                                        , Just ( setDragHandleState Hover dragHandle, distance )
                                                        )

                                                    else
                                                        ( setDragHandleState Default dragHandle :: result
                                                        , maybeNearest
                                                        )
                                )
                                ( [], Nothing )
                                testBlockBot.dragHandles
                                |> (\( defaults, maybeHover ) ->
                                        case maybeHover of
                                            Nothing ->
                                                defaults

                                            Just ( toHover, _ ) ->
                                                toHover :: defaults
                                   )
                    }
                , cursorPnt = cursorPoint
              }
            , Cmd.none
            )

        MouseDown ->
            ( model, Cmd.none )

        MouseUp ->
            ( model, Cmd.none )


setDragHandleState : DragHandleState -> DragHandle -> DragHandle
setDragHandleState state handle =
    { sphere = handle.sphere
    , state = state
    , entity =
        Scene3d.sphere
            (Scene3d.Material.color <|
                case state of
                    Default ->
                        Color.white

                    Hover ->
                        Color.blue

                    Dragging ->
                        Color.red
            )
            handle.sphere
    }


view : Model -> Browser.Document Msg
view model =
    { title = "Block Builder"
    , body =
        [ Html.div
            [ Html.Attributes.style "display" "inline-block"
            , Html.Events.on "mousemove" decodeMouseMove
            , Html.Events.onMouseDown MouseDown
            , Html.Events.onMouseUp MouseUp
            ]
            [ Scene3d.sunny
                { upDirection = Direction3d.positiveZ
                , sunlightDirection = Direction3d.negativeZ
                , shadows = True
                , dimensions = ( Pixels.int 800, Pixels.int 600 )
                , camera = model.camera
                , clipDepth = Length.millimeter
                , background = Scene3d.backgroundColor Color.lightBlue
                , entities =
                    List.concat
                        [ [ model.testBlockBot.entity ]
                        , model.testBlockBot.dragHandles
                            |> List.map (\dragHandle -> dragHandle.entity)
                        ]
                }
            ]
        ]
    }
