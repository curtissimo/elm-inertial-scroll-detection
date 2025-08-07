module Curtissimo.IntertialScrollDetector exposing
    ( onInertialScroll, init, update
    , InertialDirection(..), inertialX, inertialY, stickyX, stickyY, scrollLeft, scrollTop
    , Msg, ScrollState
    )

{-| This module implements an inertial scroll detector to provide
an event-driven mechanism to know when inertial scrolling occurs.

This is the directed state diagram for the inertial scroll detector.

![State Diagram](https://www.plantuml.com/plantuml/svg/TOynJmCn38Nt_0fBB81qOk-0YaG7XYAaCe0GGxXOEOl4hdm-4F-UL6hTBDsIzvwVFwzdYsfBBM6YX81rRt-GS1wLzaR28JoAriW6AzWPbH8UVkCt-czJADDNEGjROJ9HuknIQ3iCuObHZMA1gpOpMLP43Ywku4LcClf148LdpF7mxlqtY9pkclaUmiVj9mptS6Hpng5f9xvNMSQf4KIr8_Dmdd1_mwbpJzUulPN6u6fBTEVr5PrywEceTwDAAN1DjMAYQ7ZJXIy2Uq7Fx6LjeoSkLgoHqrBBFm00)


# Setting up

@docs onInertialScroll, init, update


# Usage

@docs InertialDirection, inertialX, inertialY, stickyX, stickyY, scrollLeft, scrollTop


# Opaque types

@docs Msg, ScrollState

-}

import Html
import Html.Events
import Json.Decode


type alias DataValue =
    { current : EventData
    , directionX : InertialDirection
    , directionY : InertialDirection
    , isTouched : Bool
    , previous : EventData
    , stickyX : InertialDirection
    , stickyY : InertialDirection
    , wasMoved : Bool
    }


type alias EventData =
    { timeStamp : Int
    , x : Int
    , y : Int
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
    | TouchMove EventData
    | TouchStart


{-| The state maintained by the detector.
-}
type ScrollState
    = ScrollState DataValue


defaultValues :
    Int
    -> Int
    ->
        { current : EventData
        , directionX : InertialDirection
        , directionY : InertialDirection
        , isTouched : Bool
        , previous : EventData
        , stickyX : InertialDirection
        , stickyY : InertialDirection
        , wasMoved : Bool
        }
defaultValues x y =
    { current = { x = x, y = y, timeStamp = eventWindow }
    , directionX = Still
    , directionY = Still
    , isTouched = False
    , previous = { x = 0, y = 0, timeStamp = 0 }
    , stickyX = Still
    , stickyY = Still
    , wasMoved = False
    }


eventWindow : Int
eventWindow =
    1000


{-| Get the current inertial scroll in the X (horizontal) direction.
-}
inertialX : ScrollState -> InertialDirection
inertialX (ScrollState data) =
    data.directionX


{-| Get the current inertial scroll in the Y (vertical) direction.
-}
inertialY : ScrollState -> InertialDirection
inertialY (ScrollState data) =
    data.directionY


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


eventDecoder : (EventData -> Msg) -> (Msg -> msg) -> Json.Decode.Decoder ( msg, Bool )
eventDecoder msg contentMsg =
    Json.Decode.map3 EventData
        (Json.Decode.at [ "timeStamp" ] Json.Decode.int)
        (Json.Decode.at [ "currentTarget", "scrollLeft" ] Json.Decode.int)
        (Json.Decode.at [ "currentTarget", "scrollTop" ] Json.Decode.int)
        |> Json.Decode.andThen (msg >> contentMsg >> pairWith False >> Json.Decode.succeed)


scrollDecoder : (Msg -> msg) -> Json.Decode.Decoder ( msg, Bool )
scrollDecoder =
    eventDecoder Scroll


scrollEndDecoder : (Msg -> msg) -> Json.Decode.Decoder ( msg, Bool )
scrollEndDecoder =
    eventDecoder ScrollEnd


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


{-| Get the last non-`Still` inertial scroll in the X (horizontal) direction.

Will return `Still` if the scrollable area has never moved.

-}
stickyX : ScrollState -> InertialDirection
stickyX (ScrollState data) =
    data.stickyX


{-| Get the last non-`Still` inertial scroll in the Y (vertical) direction.

Will return `Still` if the scrollable area has never moved.

-}
stickyY : ScrollState -> InertialDirection
stickyY (ScrollState data) =
    data.stickyY


touchEndDecoder : (Msg -> msg) -> Json.Decode.Decoder ( msg, Bool )
touchEndDecoder contentMsg =
    Json.Decode.succeed ( contentMsg TouchEnd, False )


touchMoveDecoder : (Msg -> msg) -> Json.Decode.Decoder ( msg, Bool )
touchMoveDecoder =
    eventDecoder TouchMove


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
                    { directionX, directionY, stickyX_, stickyY_, wasMoved } =
                        updateData data model
                in
                ScrollState
                    { model
                        | current = data
                        , directionX = directionX
                        , directionY = directionY
                        , previous = model.current
                        , stickyX = stickyX_
                        , stickyY = stickyY_
                        , wasMoved = wasMoved
                    }

        ScrollEnd data ->
            ScrollState
                { model
                    | wasMoved = False
                    , previous = model.current
                    , current = data
                    , directionX = Still
                    , directionY = Still
                }

        TouchEnd ->
            ScrollState { model | isTouched = False }

        TouchMove data ->
            let
                { directionX, directionY, stickyX_, stickyY_ } =
                    updateData data model
            in
            ScrollState
                { model
                    | wasMoved = True
                    , previous = model.current
                    , current = data
                    , directionX = directionX
                    , directionY = directionY
                    , stickyX = stickyX_
                    , stickyY = stickyY_
                }

        TouchStart ->
            ScrollState { model | isTouched = True, wasMoved = False }


updateData :
    EventData
    -> DataValue
    ->
        { directionX : InertialDirection
        , directionY : InertialDirection
        , stickyX_ : InertialDirection
        , stickyY_ : InertialDirection
        , wasMoved : Bool
        }
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
                Still

        directionY =
            if trackDirection && data.y - model.current.y < 0 then
                Negative

            else if trackDirection && data.y - model.current.y > 0 then
                Positive

            else
                Still

        stickyX_ =
            case directionX of
                Still ->
                    model.stickyX

                _ ->
                    directionX

        stickyY_ =
            case directionY of
                Still ->
                    model.stickyY

                _ ->
                    directionY
    in
    { directionX = directionX
    , directionY = directionY
    , stickyX_ = stickyX_
    , stickyY_ = stickyY_
    , wasMoved = wasMoved
    }
