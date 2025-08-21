module Curtissimo.IntertialScrollDetector exposing
    ( onInertialScroll, init, update
    , InertialDirection(..), inertialX, inertialY, stickyX, stickyY, scrollLeft, scrollTop
    , map
    , Msg, ScrollState
    )

{-| This module implements an inertial scroll detector to provide
an event-driven mechanism to know when inertial scrolling occurs.

This is the directed state diagram for the inertial scroll detector.

![State Diagram](https://www.plantuml.com/plantuml/svg/ZOyn2y8m48Nt-nKt2nrT7AIhGoVTnA4qXpr8SYMzYlvxqq1J217SadlVU--kdRKFkZWB8nj2SfD-qD0y19wKnGFS3mzicZDsubabi7_2nLDGxkrrzP99yZR34qQpM5kbRh8C8va8wVfAvHaiQd8IUPEPtocxyM-cazTEkFY1D4bQiVr6hRjiqTM8GirzkxzIaa_5DnwXZT3Yx1S0)


# Setting up

@docs onInertialScroll, init, update


# Usage

@docs InertialDirection, inertialX, inertialY, stickyX, stickyY, scrollLeft, scrollTop


# Mapping

@docs map


# Opaque types

@docs Msg, ScrollState

-}

import Html
import Html.Events
import Json.Decode
import Process
import Task


type alias EventAxisData =
    { bound : Int
    , offset : Int
    , viewport : Int
    }


type alias EventData =
    { x : EventAxisData
    , y : EventAxisData
    , timestamp : Int
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
    = CheckScrollEnd Int
    | Scroll EventData
    | ScrollEnd EventData
    | TouchEnd EventData
    | TouchMove EventData
    | TouchStart EventData


type alias StillStateAxisData =
    { offset : Int
    , sticky : InertialDirection
    }


type alias StillState msg =
    { count : Int
    , horizontal : StillStateAxisData
    , vertical : StillStateAxisData
    , timestamp : Int
    , toContentMsg : Msg -> msg
    }


type alias InertialStateAxisData =
    { offset : Int
    , inertial : InertialDirection
    , sticky : InertialDirection
    }


type alias InertialState msg =
    { count : Int
    , horizontal : InertialStateAxisData
    , vertical : InertialStateAxisData
    , timestamp : Int
    , toContentMsg : Msg -> msg
    }


{-| The state maintained by the detector.
-}
type ScrollState msg
    = InertialStarted (InertialState msg)
    | Inertial (InertialState msg)
    | Monitoring (StillState msg)
    | Moved (StillState msg)
    | Touched (StillState msg)


xDecoder : Json.Decode.Decoder EventAxisData
xDecoder =
    Json.Decode.map3 EventAxisData
        (Json.Decode.at [ "currentTarget", "scrollWidth" ] Json.Decode.int)
        (Json.Decode.at [ "currentTarget", "scrollLeft" ] Json.Decode.int)
        (Json.Decode.at [ "currentTarget", "clientWidth" ] Json.Decode.int)


yDecoder : Json.Decode.Decoder EventAxisData
yDecoder =
    Json.Decode.map3 EventAxisData
        (Json.Decode.at [ "currentTarget", "scrollHeight" ] Json.Decode.int)
        (Json.Decode.at [ "currentTarget", "scrollTop" ] Json.Decode.int)
        (Json.Decode.at [ "currentTarget", "clientHeight" ] Json.Decode.int)


eventDecoder : (EventData -> Msg) -> ScrollState msg -> Json.Decode.Decoder ( msg, Bool )
eventDecoder msg state =
    Json.Decode.map3 EventData
        xDecoder
        yDecoder
        (Json.Decode.at [ "timeStamp" ] Json.Decode.int)
        |> Json.Decode.andThen (msg >> stateToContentMsg state >> pairWith False >> Json.Decode.succeed)


eventData : Msg -> EventData
eventData msg =
    case msg of
        CheckScrollEnd _ ->
            { x = { bound = 0, offset = 0, viewport = 0 }
            , y = { bound = 0, offset = 0, viewport = 0 }
            , timestamp = 0
            }

        Scroll data ->
            data

        ScrollEnd data ->
            data

        TouchEnd data ->
            data

        TouchMove data ->
            data

        TouchStart data ->
            data


eventWindow : Float
eventWindow =
    300.0


inertialToStill : EventData -> InertialState msg -> StillState msg
inertialToStill values state =
    { count = 1
    , horizontal = { offset = values.x.offset, sticky = state.horizontal.sticky }
    , vertical = { offset = values.y.offset, sticky = state.vertical.sticky }
    , timestamp = values.timestamp
    , toContentMsg = state.toContentMsg
    }


{-| Get the current inertial scroll in the X (horizontal) direction.
-}
inertialX : ScrollState msg -> InertialDirection
inertialX state =
    case state of
        Inertial data ->
            data.horizontal.inertial

        _ ->
            Still


{-| Get the current inertial scroll in the Y (vertical) direction.
-}
inertialY : ScrollState msg -> InertialDirection
inertialY state =
    case state of
        Inertial data ->
            data.vertical.inertial

        _ ->
            Still


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
init : Int -> Int -> (Msg -> msg) -> ScrollState msg
init left top toContentMsg =
    Monitoring
        { count = 1
        , horizontal = { offset = left, sticky = Still }
        , vertical = { offset = top, sticky = Still }
        , timestamp = -1
        , toContentMsg = toContentMsg
        }


{-| Map a [`ScrollState`](#ScrollState) from one msg type
to another.
-}
map : (msg1 -> msg2) -> ScrollState msg1 -> ScrollState msg2
map toMsg state =
    case state of
        InertialStarted data ->
            let
                horizontal =
                    data.horizontal

                vertical =
                    data.vertical
            in
            InertialStarted
                { count = data.count
                , horizontal = horizontal
                , vertical = vertical
                , timestamp = data.timestamp
                , toContentMsg = data.toContentMsg >> toMsg
                }

        Inertial data ->
            let
                horizontal =
                    data.horizontal

                vertical =
                    data.vertical
            in
            Inertial
                { count = data.count
                , horizontal = horizontal
                , vertical = vertical
                , timestamp = data.timestamp
                , toContentMsg = data.toContentMsg >> toMsg
                }

        Monitoring data ->
            let
                horizontal =
                    data.horizontal

                vertical =
                    data.vertical
            in
            Monitoring
                { count = data.count
                , horizontal = horizontal
                , vertical = vertical
                , timestamp = data.timestamp
                , toContentMsg = data.toContentMsg >> toMsg
                }

        Moved data ->
            let
                horizontal =
                    data.horizontal

                vertical =
                    data.vertical
            in
            Moved
                { count = data.count
                , horizontal = horizontal
                , vertical = vertical
                , timestamp = data.timestamp
                , toContentMsg = data.toContentMsg >> toMsg
                }

        Touched data ->
            let
                horizontal =
                    data.horizontal

                vertical =
                    data.vertical
            in
            Touched
                { count = data.count
                , horizontal = horizontal
                , vertical = vertical
                , timestamp = data.timestamp
                , toContentMsg = data.toContentMsg >> toMsg
                }


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
onInertialScroll : ScrollState msg -> List (Html.Attribute msg)
onInertialScroll state =
    [ Html.Events.stopPropagationOn "scroll" (eventDecoder Scroll state)
    , Html.Events.stopPropagationOn "scrollend" (eventDecoder ScrollEnd state)
    , Html.Events.stopPropagationOn "touchstart" (eventDecoder TouchStart state)
    , Html.Events.stopPropagationOn "touchmove" (eventDecoder TouchMove state)
    , Html.Events.stopPropagationOn "touchend" (eventDecoder TouchEnd state)
    ]


pairWith : b -> a -> ( a, b )
pairWith b a =
    ( a, b )


{-| Get the current left scroll position from the latest inertial scroll.
-}
scrollLeft : ScrollState msg -> Int
scrollLeft state =
    case state of
        InertialStarted data ->
            data.horizontal.offset

        Inertial data ->
            data.horizontal.offset

        Monitoring data ->
            data.horizontal.offset

        Moved data ->
            data.horizontal.offset

        Touched data ->
            data.horizontal.offset


{-| Get the current top scroll position from the latest inertial scroll.
-}
scrollTop : ScrollState msg -> Int
scrollTop state =
    case state of
        InertialStarted data ->
            data.vertical.offset

        Inertial data ->
            data.vertical.offset

        Monitoring data ->
            data.vertical.offset

        Moved data ->
            data.vertical.offset

        Touched data ->
            data.vertical.offset


stateToContentMsg : ScrollState msg -> (Msg -> msg)
stateToContentMsg state =
    case state of
        InertialStarted data ->
            data.toContentMsg

        Inertial data ->
            data.toContentMsg

        Monitoring data ->
            data.toContentMsg

        Moved data ->
            data.toContentMsg

        Touched data ->
            data.toContentMsg


stateTimestamp : ScrollState msg -> Int
stateTimestamp state =
    case state of
        InertialStarted data ->
            data.timestamp

        Inertial data ->
            data.timestamp

        Monitoring data ->
            data.timestamp

        Moved data ->
            data.timestamp

        Touched data ->
            data.timestamp


{-| Get the last non-`Still` inertial scroll in the X (horizontal) direction.

Will return `Still` if the scrollable area has never moved.

-}
stickyX : ScrollState msg -> InertialDirection
stickyX state =
    case state of
        InertialStarted data ->
            data.horizontal.sticky

        Inertial data ->
            data.horizontal.sticky

        Monitoring data ->
            data.horizontal.sticky

        Moved data ->
            data.horizontal.sticky

        Touched data ->
            data.horizontal.sticky


{-| Get the last non-`Still` inertial scroll in the Y (vertical) direction.

Will return `Still` if the scrollable area has never moved.

-}
stickyY : ScrollState msg -> InertialDirection
stickyY state =
    case state of
        InertialStarted data ->
            data.vertical.sticky

        Inertial data ->
            data.vertical.sticky

        Monitoring data ->
            data.vertical.sticky

        Moved data ->
            data.vertical.sticky

        Touched data ->
            data.vertical.sticky


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
update : Msg -> ScrollState msg -> ( ScrollState msg, Cmd msg )
update msg state =
    case ( state, msg ) of
        ( Inertial current, CheckScrollEnd timestamp ) ->
            if timestamp < current.timestamp then
                ( state, Cmd.none )

            else
                let
                    data =
                        { x = { bound = 0, offset = current.horizontal.offset, viewport = 0 }
                        , y = { bound = 0, offset = current.vertical.offset, viewport = 0 }
                        , timestamp = stateTimestamp state
                        }
                in
                update (ScrollEnd data) state

        ( InertialStarted current, Scroll event ) ->
            let
                horizontalInertial =
                    updateInertialDirection current.horizontal event.x

                verticalInertial =
                    updateInertialDirection current.vertical event.y
            in
            ( Inertial
                { count = 1
                , horizontal =
                    { offset = event.x.offset
                    , inertial = horizontalInertial
                    , sticky = stickyInertialDirection current.horizontal.sticky horizontalInertial
                    }
                , vertical =
                    { offset = event.y.offset
                    , inertial = verticalInertial
                    , sticky = stickyInertialDirection current.vertical.sticky verticalInertial
                    }
                , timestamp = event.timestamp
                , toContentMsg = current.toContentMsg
                }
            , Cmd.none
            )

        ( InertialStarted current, TouchStart event ) ->
            ( Touched (inertialToStill event current)
            , Cmd.none
            )

        ( Inertial current, Scroll event ) ->
            let
                horizontalInertial =
                    updateInertialDirection current.horizontal event.x

                verticalInertial =
                    updateInertialDirection current.vertical event.y
            in
            ( Inertial
                { count = current.count + 1
                , horizontal =
                    { offset = event.x.offset
                    , inertial = horizontalInertial
                    , sticky = stickyInertialDirection current.horizontal.sticky horizontalInertial
                    }
                , vertical =
                    { offset = event.y.offset
                    , inertial = verticalInertial
                    , sticky = stickyInertialDirection current.vertical.sticky verticalInertial
                    }
                , timestamp = event.timestamp
                , toContentMsg = current.toContentMsg
                }
            , Process.sleep eventWindow
                |> Task.perform (\_ -> CheckScrollEnd event.timestamp |> current.toContentMsg)
            )

        ( Inertial current, ScrollEnd event ) ->
            ( Monitoring (inertialToStill event current)
            , Cmd.none
            )

        ( Inertial current, TouchStart event ) ->
            ( Touched (inertialToStill event current)
            , Cmd.none
            )

        ( Monitoring current, TouchStart event ) ->
            ( Touched (updateStillState 1 event current)
            , Cmd.none
            )

        ( Monitoring current, TouchMove event ) ->
            ( Moved (updateStillState 1 event current)
            , Cmd.none
            )

        ( Moved current, TouchEnd event ) ->
            let
                horizontal =
                    { offset = current.horizontal.offset, inertial = Still, sticky = current.horizontal.sticky }

                vertical =
                    { offset = current.vertical.offset, inertial = Still, sticky = current.vertical.sticky }

                horizontalInertial =
                    updateInertialDirection horizontal event.x

                verticalInertial =
                    updateInertialDirection vertical event.y
            in
            ( InertialStarted
                { count = 1
                , horizontal =
                    { offset = event.x.offset
                    , inertial = horizontalInertial
                    , sticky = stickyInertialDirection current.horizontal.sticky horizontalInertial
                    }
                , vertical =
                    { offset = event.y.offset
                    , inertial = verticalInertial
                    , sticky = stickyInertialDirection current.vertical.sticky verticalInertial
                    }
                , timestamp = event.timestamp
                , toContentMsg = current.toContentMsg
                }
            , Cmd.none
            )

        ( Touched current, TouchMove event ) ->
            ( Moved (updateStillState 1 event current)
            , Cmd.none
            )

        ( Touched current, TouchEnd event ) ->
            ( Monitoring (updateStillState 1 event current)
            , Cmd.none
            )

        ( _, Scroll _ ) ->
            ( updateGeneral { fromScroll = True, values = eventData msg } state
            , Cmd.none
            )

        _ ->
            ( updateGeneral { fromScroll = False, values = eventData msg } state
            , Cmd.none
            )


updateInertialDirection : InertialStateAxisData -> EventAxisData -> InertialDirection
updateInertialDirection axis event =
    let
        upperBound =
            event.bound - event.viewport

        eventOutsideViewBounds =
            event.offset <= 0 || event.offset >= upperBound

        diff =
            event.offset - axis.offset
    in
    if eventOutsideViewBounds then
        axis.inertial

    else if diff < 0 then
        Negative

    else if diff > 0 then
        Positive

    else
        Still


stickyInertialDirection : InertialDirection -> InertialDirection -> InertialDirection
stickyInertialDirection from to =
    case ( from, to ) of
        ( Still, _ ) ->
            to

        ( _, Still ) ->
            from

        _ ->
            to


updateStillState : Int -> EventData -> StillState msg -> StillState msg
updateStillState count values state =
    let
        horizontal =
            state.horizontal

        vertical =
            state.vertical
    in
    { state
        | count = count
        , horizontal = { horizontal | offset = values.x.offset }
        , vertical = { vertical | offset = values.y.offset }
        , timestamp = values.timestamp
    }


updateGeneral : { fromScroll : Bool, values : EventData } -> ScrollState msg -> ScrollState msg
updateGeneral { fromScroll, values } state =
    case state of
        Inertial data ->
            let
                horizontal =
                    data.horizontal

                vertical =
                    data.vertical
            in
            Inertial
                { data
                    | count = data.count + 1
                    , horizontal = { horizontal | offset = values.x.offset }
                    , vertical = { vertical | offset = values.y.offset }
                    , timestamp = values.timestamp
                }

        InertialStarted data ->
            let
                horizontal =
                    data.horizontal

                vertical =
                    data.vertical
            in
            InertialStarted
                { data
                    | count = data.count + 1
                    , horizontal = { horizontal | offset = values.x.offset }
                    , vertical = { vertical | offset = values.y.offset }
                    , timestamp = values.timestamp
                }

        Monitoring data ->
            Monitoring (updateStillState (data.count + 1) values data)

        Moved data ->
            -- Sometimes, touchend doesn't fire. Watch scrolled in moved.
            -- When it happens "a lot," transition to Inertial state.
            if fromScroll && data.count > 5 then
                let
                    horizontal =
                        { offset = data.horizontal.offset, inertial = Still, sticky = data.horizontal.sticky }

                    vertical =
                        { offset = data.vertical.offset, inertial = Still, sticky = data.vertical.sticky }

                    horizontalInertial =
                        updateInertialDirection horizontal values.x

                    verticalInertial =
                        updateInertialDirection vertical values.y
                in
                Inertial
                    { count = 1
                    , horizontal =
                        { offset = values.x.offset
                        , inertial = horizontalInertial
                        , sticky = stickyInertialDirection data.horizontal.sticky horizontalInertial
                        }
                    , vertical =
                        { offset = values.y.offset
                        , inertial = verticalInertial
                        , sticky = stickyInertialDirection data.vertical.sticky verticalInertial
                        }
                    , timestamp = values.timestamp
                    , toContentMsg = data.toContentMsg
                    }

            else if fromScroll then
                Moved (updateStillState (data.count + 1) values data)

            else
                Moved (updateStillState 1 values data)

        Touched data ->
            Touched (updateStillState (data.count + 1) values data)
