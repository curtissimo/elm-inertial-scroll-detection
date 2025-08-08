`elm-inertial-scroll-detection` [![Build Status](https://github.com/curtissimo/elm-inertial-scroll-detection/workflows/CI/badge.svg)](https://github.com/curtissimo/elm-inertial-scroll-detection/actions?query=branch%3Amain)

Do you want to show and hide navigation bars based on inertial scrolling
just like native apps do?

If so, then this package is for you!

## Examples

Check out the `Scroll` live example on 
[GitLab Pages](https://elm-inertial-scroll-detection-9e103d.gitlab.io/).

Or, check out the 
[./examples/README.md](https://gitlab.com/curtissimo/elm-inertial-scroll-detection/-/tree/2.0.1/examples).

## Scrollable HTML elements

You need to make sure the scrollable element your tracking has its CSS set
correctly to allow inertial scrolling. After that, you can effectively use
the inertial scroll detector.

### CSS

For an element to be scrollable, you need to set a height for the element
as well as set its `overflow` style property. Something like this.

```css
.scrollable {
  height: 400px;
  overflow: auto;
}
```

### Using `Curtissimo.InertialScrollDetector`

The detector is a stateful component, so needs to be wired up using your 
model, `update`, and `view` functions.

```elm
import Curtissimo.IntertialScrollDetector as Detector
import Html
import Html.Attributes as Attrs

-- Your element, sandbox, application

type alias Model =
    { scrollState : Detector.ScrollState }

init =
    ({ scrollState = Detector.init 0 0 }
    , Cmd.none
    )

type Msg
    = InertialScroll Detector.Msg

update msg model =
    case msg of
        InertialScroll scrollMsg ->
            ( { scrollState
                | scrollState = Detector.update scrollMsg
              }
            , Cmd.none 
            )

view model =
    Html.div 
      [ Attrs.class "scrollable"
      , Detector.onInertialScroll InertialScroll
      ]
      (List.repeat 200 (Html.p [] [Html.text "Scroll me!"]))
```
