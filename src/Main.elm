module Main exposing (main)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Block3d
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Dict exposing (Dict)
import Direction3d
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Length
import Pixels exposing (Pixels)
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)
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
    , cursorPoint : Point2d Pixels ScreenCoordinates
    , dragging : Maybe Int
    }


type alias Ent =
    { entity : Scene3d.Entity GameCoordinates
    , parts : EntParts
    , dragHandles : Dict Int DragHandle
    }


type alias DragHandle =
    { entity : Scene3d.Entity GameCoordinates
    , sphere : Sphere3d Length.Meters GameCoordinates
    , state : DragHandleState
    , dragPlane : Plane3d Length.Meters GameCoordinates
    , dragAxis : Axis3d Length.Meters GameCoordinates
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
      , cameraAzimuth = Angle.degrees 15
      , camera = setCamera (Angle.degrees 15)
      , cursorPoint = Point2d.origin
      , dragging = Nothing
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
                [ ( center
                        |> Point3d.translateIn (Axis3d.direction xAxis) width
                        |> createDragHandleSphere
                  , xAxis
                  , Plane3d.through center (Axis3d.direction zAxis)
                  )
                , ( center
                        |> Point3d.translateIn (xAxis |> Axis3d.reverse |> Axis3d.direction) width
                        |> createDragHandleSphere
                  , xAxis |> Axis3d.reverse
                  , Plane3d.through center (Axis3d.direction zAxis)
                  )
                --   
                , ( center
                        |> Point3d.translateIn (Axis3d.direction yAxis) length
                        |> createDragHandleSphere
                  , yAxis
                  , Plane3d.through center (Axis3d.direction zAxis)
                  )
                , ( center
                        |> Point3d.translateIn (yAxis |> Axis3d.reverse |> Axis3d.direction) length
                        |> createDragHandleSphere
                  , yAxis |> Axis3d.reverse
                  , Plane3d.through center (Axis3d.direction zAxis)
                  )
                --   
                , ( center
                        |> Point3d.translateIn (Axis3d.direction zAxis) height
                        |> createDragHandleSphere
                  , zAxis
                  , Plane3d.through center (Axis3d.direction xAxis)
                  )
                , ( center
                        |> Point3d.translateIn (zAxis |> Axis3d.reverse |> Axis3d.direction) height
                        |> createDragHandleSphere
                  , zAxis |> Axis3d.reverse
                  , Plane3d.through center (Axis3d.direction xAxis)
                  )
                ]
                    |> List.indexedMap
                        (\index ( sphere, dragAxis, dragPlane ) ->
                            ( index
                            , { sphere = sphere
                              , entity = Scene3d.sphere (Scene3d.Material.color Color.white) sphere
                              , state = Default
                              , dragPlane = dragPlane
                              , dragAxis = dragAxis
                              }
                            )
                        )
                    |> Dict.fromList
            }


createDragHandleSphere center =
    Sphere3d.atPoint center (Length.meters 0.125)


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


screenRectangle2d : Rectangle2d Pixels ScreenCoordinates
screenRectangle2d =
    Rectangle2d.from
        -- bottom left
        (Point2d.pixels 0 600)
        -- top right
        (Point2d.pixels 800 0)


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
            in
            case model.dragging of
                Nothing ->
                    let
                        ray : Axis3d Length.Meters GameCoordinates
                        ray =
                            Camera3d.ray model.camera screenRectangle2d cursorPoint

                        rayStart : Point3d Length.Meters GameCoordinates
                        rayStart =
                            Axis3d.originPoint ray
                    in
                    ( { model
                        | testBlockBot =
                            { testBlockBot
                                | dragHandles =
                                    Dict.foldl
                                        (\id dragHandle ( result, maybeNearest ) ->
                                            case ( Axis3d.intersectionWithSphere dragHandle.sphere ray, dragHandle.state ) of
                                                ( Nothing, Default ) ->
                                                    ( Dict.insert id dragHandle result, maybeNearest )

                                                ( Nothing, _ ) ->
                                                    ( Dict.insert id (setDragHandleState Default dragHandle) result, maybeNearest )

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
                                                            , Just ( id, setDragHandleState Hover dragHandle, distance )
                                                            )

                                                        Just ( nearestId, nearest, nearestDist ) ->
                                                            if distance |> Quantity.lessThan nearestDist then
                                                                ( Dict.insert nearestId (setDragHandleState Default nearest) result
                                                                , Just ( id, setDragHandleState Hover dragHandle, distance )
                                                                )

                                                            else
                                                                ( Dict.insert id (setDragHandleState Default dragHandle) result
                                                                , maybeNearest
                                                                )
                                        )
                                        ( Dict.empty, Nothing )
                                        testBlockBot.dragHandles
                                        |> (\( defaults, maybeHover ) ->
                                                case maybeHover of
                                                    Nothing ->
                                                        defaults

                                                    Just ( id, toHover, _ ) ->
                                                        Dict.insert id toHover defaults
                                           )
                            }
                        , cursorPoint = cursorPoint
                      }
                    , Cmd.none
                    )

                Just draggingId ->
                    case Dict.get draggingId model.testBlockBot.dragHandles of
                        Nothing ->
                            ( model, Cmd.none )

                        Just dragHandle ->
                            let
                                newRay : Axis3d Length.Meters GameCoordinates
                                newRay =
                                    Camera3d.ray model.camera screenRectangle2d cursorPoint
                            in
                            case Axis3d.intersectionWithPlane dragHandle.dragPlane newRay of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just pnt ->
                                    let
                                        intersectionPnt : Point3d Length.Meters GameCoordinates
                                        intersectionPnt =
                                            Point3d.projectOntoAxis
                                                dragHandle.dragAxis
                                                pnt
                                    in
                                    ( { model
                                        | testBlockBot =
                                            { testBlockBot
                                                | dragHandles =
                                                    Dict.insert draggingId
                                                        (setDragHandlePosition intersectionPnt dragHandle)
                                                        testBlockBot.dragHandles
                                            }
                                        }
                                    , Cmd.none
                                    )

        MouseDown ->
            let
                hovering : Maybe ( Int, DragHandle )
                hovering =
                    model.testBlockBot.dragHandles
                        |> Dict.toList
                        |> List.filter (\( _, dragHandle ) -> dragHandle.state == Hover)
                        |> List.head
            in
            case hovering of
                Nothing ->
                    ( model, Cmd.none )

                Just ( id, dragHandle ) ->
                    let
                        testBlockBot =
                            model.testBlockBot
                    in
                    ( { model
                        | dragging = Just id
                        , testBlockBot =
                            { testBlockBot
                                | dragHandles =
                                    Dict.insert
                                        id
                                        (setDragHandleState Dragging dragHandle)
                                        testBlockBot.dragHandles
                            }
                      }
                    , Cmd.none
                    )

        MouseUp ->
            case model.dragging of
                Nothing ->
                    ( model, Cmd.none )

                Just draggingId ->
                    let
                        testBlockBot =
                            model.testBlockBot
                    in
                    ( { model
                        | dragging = Nothing
                        , testBlockBot =
                            { testBlockBot
                                | dragHandles =
                                    Dict.update draggingId
                                        (\maybeDragHandle ->
                                            case maybeDragHandle of
                                                Nothing ->
                                                    Nothing

                                                Just dragHandle ->
                                                    Just (setDragHandleState Hover dragHandle)
                                        )
                                        testBlockBot.dragHandles
                            }
                      }
                    , Cmd.none
                    )


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
    , dragPlane = handle.dragPlane
    , dragAxis = handle.dragAxis
    }


setDragHandlePosition : Point3d Length.Meters GameCoordinates -> DragHandle -> DragHandle
setDragHandlePosition center handle =
    let
        newSphere : Sphere3d Length.Meters GameCoordinates
        newSphere =
            Sphere3d.atPoint center
                (Sphere3d.radius handle.sphere)
    in
    { sphere = newSphere
    , state = handle.state
    , entity =
        Scene3d.sphere
            (Scene3d.Material.color <|
                case handle.state of
                    Default ->
                        Color.white

                    Hover ->
                        Color.blue

                    Dragging ->
                        Color.red
            )
            newSphere
    , dragPlane = handle.dragPlane
    , dragAxis = handle.dragAxis
    }


view : Model -> Browser.Document Msg
view model =
    { title = "Block Builder"
    , body =
        [ Html.div
            [ Html.Attributes.style "display" "inline-block"
            , Html.Attributes.style "cursor" <|
                case model.dragging of
                    Just _ -> "grabbing"
                    Nothing ->
                        if (Dict.filter (\_ dragHandle -> dragHandle.state == Hover) model.testBlockBot.dragHandles |> Dict.size) > 0 then
                            "grab"
                        else
                            "inherit"
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
                            |> Dict.foldl
                                (\_ dragHandle result -> dragHandle.entity :: result)
                                []
                        ]
                }
            ]
        ]
    }
