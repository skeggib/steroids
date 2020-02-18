module Gestures exposing (LongPressEvent(..), LongPressModel, initLongPress, longPress, updateLongPress)

import Html
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Process
import Task


longPress : object -> (LongPressEvent object -> msg) -> List (Html.Attribute msg)
longPress object converter =
    [ Touch.onWithOptions "touchstart"
        { stopPropagation = False, preventDefault = False }
        (\event -> Press object (touchCoordinates event) |> converter)
    , Touch.onWithOptions "touchmove"
        { stopPropagation = False, preventDefault = False }
        (\event -> Move (touchCoordinates event) |> converter)
    , Touch.onEnd (\_ -> Release |> converter)
    , Mouse.onDown (\event -> Press object (mouseCoordinates event) |> converter)
    , Mouse.onMove (\event -> Move (mouseCoordinates event) |> converter)
    , Mouse.onUp (\_ -> Release |> converter)
    ]


type LongPressEvent object
    = Press object ( Float, Float )
    | Release
    | Move ( Float, Float )
    | Timeout object Int
    | Reset


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates event =
    List.head event.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )


mouseCoordinates : Mouse.Event -> ( Float, Float )
mouseCoordinates event =
    event.offsetPos


type alias LongPressModel object =
    { pressing : Maybe object
    , pressed : Maybe object
    , index : Int
    , startPos : ( Float, Float )
    }


initLongPress : LongPressModel object
initLongPress =
    { pressing = Nothing
    , pressed = Nothing
    , index = 0
    , startPos = ( 0, 0 )
    }


updateLongPress :
    LongPressEvent object
    -> LongPressModel object
    -> (LongPressEvent object -> msg) -- TODO: replace with Cmd.map
    -> ( LongPressModel object, Cmd msg )
updateLongPress event model converter =
    case event of
        Press object pos ->
            ( { model
                | pressing = Just object
                , startPos = pos
                , index = model.index + 1
                , pressed = Nothing
              }
            , cmd event model converter
            )

        Release ->
            ( { model | pressing = Nothing }
            , cmd event model converter
            )

        Move pos ->
            if distance pos model.startPos > 50 then
                ( { model | pressing = Nothing }
                , cmd event model converter
                )

            else
                ( model
                , cmd event model converter
                )

        Timeout object index ->
            if model.pressing == Just object && model.index == index then
                ( { model
                    | pressed = Just object
                    , pressing = Nothing
                  }
                , cmd event model converter
                )

            else
                ( model
                , cmd event model converter
                )

        Reset ->
            ( { model | pressed = Nothing }
            , cmd event model converter
            )


cmd :
    LongPressEvent object
    -> LongPressModel object
    -> (LongPressEvent object -> msg)
    -> Cmd msg
cmd event model converter =
    case event of
        Press object _ ->
            Process.sleep 750 |> Task.perform (\_ -> Timeout object (model.index + 1) |> converter)

        Release ->
            Cmd.none

        Move _ ->
            Cmd.none

        Timeout _ _ ->
            Cmd.none

        Reset ->
            Cmd.none


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance p1 p2 =
    (Tuple.first p1 - Tuple.first p2) ^ 2 + (Tuple.second p1 - Tuple.second p2) ^ 2 |> sqrt
