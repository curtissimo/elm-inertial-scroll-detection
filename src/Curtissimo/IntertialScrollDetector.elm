module Curtissimo.IntertialScrollDetector exposing
    ( onInertialScroll, init, update
    , InertialDirection(..), inertialScrollX, inertialScrollY, scrollLeft, scrollTop
    , Msg, ScrollState
    )

{-| This module implements an inertial scroll detector to provide
an event-driven mechanism to know when inertial scrolling occurs.

This is the directed state diagram for the inertial scroll detector.

![State Diagram](https://www.plantuml.com/plantuml/svg/TOynJmCn38Nt_0fBB81qOk-0YaG7XYAaCe0GGxXOEOl4hdm-4F-UL6hTBDsIzvwVFwzdYsfBBM6YX81rRt-GS1wLzaR28JoAriW6AzWPbH8UVkCt-czJADDNEGjROJ9HuknIQ3iCuObHZMA1gpOpMLP43Ywku4LcClf148LdpF7mxlqtY9pkclaUmiVj9mptS6Hpng5f9xvNMSQf4KIr8_Dmdd1_mwbpJzUulPN6u6fBTEVr5PrywEceTwDAAN1DjMAYQ7ZJXIy2Uq7Fx6LjeoSkLgoHqrBBFm00)


# Setting up

@docs onInertialScroll, init, update


# Usage

@docs InertialDirection, inertialScrollX, inertialScrollY, scrollLeft, scrollTop


# Opaque types

@docs Msg, ScrollState

-}

import Html
import Html.Events
import Json.Decode


type alias DataValue =
    { isTouched : Bool
    , wasMoved : Bool
    , previous : EventData
    , current : EventData
    }


type alias EventData =
    { x : Int
    , y : Int
    , timeStamp : Int
    , directionX : InertialDirection
    , directionY : InertialDirection
    }


{-| The direction of the inertial scroll.

  - **Negative** means the scroll top (or left) is decreasing.
  - **Positive** means the scroll top (or left) is increasing.
  - **Still** means there is no movement.

-}
type InertialDirection
    = Negative
    | Positive
    | Still


{-| The messages used to monitor the scrollable area.
-}
type Msg
    = Scroll EventData
    | ScrollEnd EventData
    | TouchEnd
    | TouchMove
    | TouchStart


{-| The state maintained by the detector.
-}
type ScrollState
    = ScrollState DataValue


defaultValues :
    Int
    -> Int
    ->
        { isTouched : Bool
        , wasMoved : Bool
        , previous : EventData
        , current : EventData
        }
defaultValues x y =
    { isTouched = False
    , wasMoved = False
    , previous = { x = 0, y = 0, timeStamp = 0, directionX = Still, directionY = Still }
    , current = { x = x, y = y, timeStamp = eventWindow, directionX = Still, directionY = Still }
    }


{-| Get the current inertial scroll in the X (horizontal) direction.
-}
inertialScrollX : ScrollState -> InertialDirection
inertialScrollX (ScrollState data) =
    data.current.directionX


{-| Get the current inertial scroll in the Y (vertical) direction.
-}
inertialScrollY : ScrollState -> InertialDirection
inertialScrollY (ScrollState data) =
    data.current.directionY


{-| Initialize a [`ScrollState`](#ScrollState) from current
scroll offset left and top values.

    import Curtissimo.IntertialScrollDetector as Detector

    type alias Model =
        { scrollState : Detector.ScrollState }

    init =
        ( scrollState = Detector.init 0 0
        , Cmd.none
        )

-}
init : Int -> Int -> ScrollState
init left top =
    ScrollState (defaultValues left top)


{-| Adds the following passive event listeners to the scrollable area.

  - `scroll` to monitor for scroll events after touch-end
  - `scrollend` to monitor for when inertial scrolling ends
  - `touchstart` to activate that detector
  - `touchmove` to transition the detector to an eligible state for inertial scrolling
  - `touchend` to transition the detector to a montoring state for inertial scrolling

```
import Curtissimo.IntertialScrollDetector as Detector
import Html
import Html.Attributes as Attrs

type Msg
    = InertialScroll Detector.Msg

view =
    Html.div
        [ Attrs.class "scrollable"
        , Detector.onInertialScroll InertialScroll
        ]
        [ Html.p [] [ Html.text "Scroll me!" ] ]
```

-}
onInertialScroll : (Msg -> msg) -> List (Html.Attribute msg)
onInertialScroll contentMsg =
    [ Html.Events.stopPropagationOn "scroll" (scrollDecoder contentMsg)
    , Html.Events.stopPropagationOn "scrollend" (scrollEndDecoder contentMsg)
    , Html.Events.stopPropagationOn "touchstart" (touchStartDecoder contentMsg)
    , Html.Events.stopPropagationOn "touchmove" (touchMoveDecoder contentMsg)
    , Html.Events.stopPropagationOn "touchend" (touchEndDecoder contentMsg)
    ]


pairWith : b -> a -> ( a, b )
pairWith b a =
    ( a, b )


scrollDecoder : (Msg -> msg) -> Json.Decode.Decoder ( msg, Bool )
scrollDecoder contentMsg =
    Json.Decode.map5 EventData
        (Json.Decode.at [ "target", "scrollLeft" ] Json.Decode.int)
        (Json.Decode.at [ "target", "scrollTop" ] Json.Decode.int)
        (Json.Decode.at [ "timeStamp" ] Json.Decode.int)
        (Json.Decode.succeed Still)
        (Json.Decode.succeed Still)
        |> Json.Decode.andThen (Scroll >> contentMsg >> pairWith False >> Json.Decode.succeed)


scrollEndDecoder : (Msg -> msg) -> Json.Decode.Decoder ( msg, Bool )
scrollEndDecoder contentMsg =
    Json.Decode.map5 EventData
        (Json.Decode.at [ "target", "scrollLeft" ] Json.Decode.int)
        (Json.Decode.at [ "target", "scrollTop" ] Json.Decode.int)
        (Json.Decode.at [ "timeStamp" ] Json.Decode.int)
        (Json.Decode.succeed Still)
        (Json.Decode.succeed Still)
        |> Json.Decode.andThen (ScrollEnd >> contentMsg >> pairWith False >> Json.Decode.succeed)


{-| Get the current left scroll position from the latest inertial scroll.
-}
scrollLeft : ScrollState -> Int
scrollLeft (ScrollState { current }) =
    current.x


{-| Get the current top scroll position from the latest inertial scroll.
-}
scrollTop : ScrollState -> Int
scrollTop (ScrollState { current }) =
    current.y


eventWindow : Int
eventWindow =
    1000


touchEndDecoder : (Msg -> msg) -> Json.Decode.Decoder ( msg, Bool )
touchEndDecoder contentMsg =
    Json.Decode.succeed ( contentMsg TouchEnd, False )


touchMoveDecoder : (Msg -> msg) -> Json.Decode.Decoder ( msg, Bool )
touchMoveDecoder contentMsg =
    Json.Decode.succeed ( contentMsg TouchMove, False )


touchStartDecoder : (Msg -> msg) -> Json.Decode.Decoder ( msg, Bool )
touchStartDecoder contentMsg =
    Json.Decode.succeed ( contentMsg TouchStart, False )


{-| Update the detector from the events occurring in the window.

See [`onInertialScroll`](#onInertialScroll) for the events the detector uses to monitor for inertial scrolling.

    import Curtissimo.IntertialScrollDetector as Detector

    update msg model =
        case msg of
            InertialScroll scrollMsg ->
                ( { model
                    | scrollState = Detector.update scrollMsg
                  }
                , Cmd.none
                )

-}
update : Msg -> ScrollState -> ScrollState
update msg (ScrollState model) =
    case msg of
        Scroll data ->
            if data.timeStamp == model.current.timeStamp then
                ScrollState model

            else
                let
                    ( current, wasMoved ) =
                        updateData data model
                in
                ScrollState
                    { model
                        | previous = model.current
                        , current = current
                        , wasMoved = wasMoved
                    }

        ScrollEnd data ->
            let
                ( current, _ ) =
                    updateData data model
            in
            ScrollState
                { model
                    | wasMoved = False
                    , previous = model.current
                    , current = current
                }

        TouchEnd ->
            ScrollState { model | isTouched = False }

        TouchMove ->
            ScrollState { model | wasMoved = True }

        TouchStart ->
            ScrollState { model | isTouched = True, wasMoved = False }


updateData : EventData -> DataValue -> ( EventData, Bool )
updateData data model =
    let
        inWindow =
            data.timeStamp - model.current.timeStamp < eventWindow

        wasMoved =
            model.wasMoved && inWindow

        trackDirection =
            wasMoved && not model.isTouched && inWindow

        directionX =
            if trackDirection && data.x - model.current.x < 0 then
                Negative

            else if trackDirection && data.x - model.current.x > 0 then
                Positive

            else
                model.current.directionX

        directionY =
            if trackDirection && data.y - model.current.y < 0 then
                Negative

            else if trackDirection && data.y - model.current.y > 0 then
                Positive

            else
                model.current.directionY

        current =
            { data | directionX = directionX, directionY = directionY }
    in
    ( current, wasMoved )
