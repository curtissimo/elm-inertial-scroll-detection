module Scroll exposing (Model, Msg, main)

import Browser
import Curtissimo.IntertialScrollDetector as Detector
import Html exposing (Html)
import Html.Attributes as Attrs


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type Msg
    = Scrolled Detector.Msg


type alias Model =
    { scrollState : Detector.ScrollState }


init : Model
init =
    { scrollState = Detector.init 0 0 }


directionToString : Detector.InertialDirection -> String
directionToString direction =
    case direction of
        Detector.Negative ->
            "Negative"

        Detector.Positive ->
            "Positive"

        Detector.Still ->
            "Still"


update : Msg -> Model -> Model
update msg model =
    case msg of
        Scrolled scrollMsg ->
            { model | scrollState = Detector.update scrollMsg model.scrollState }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.node "meta" [ Attrs.name "viewport", Attrs.attribute "content" "width=device-width, initial-scale=1.0" ] []
        , Html.node "link"
            [ Attrs.href "https://cdn.jsdelivr.net/npm/bulma@1.0.4/css/bulma.min.css"
            , Attrs.rel "stylesheet"
            ]
            []
        , Html.node "style" [] [ Html.text style ]
        , Html.section [ Attrs.class "section content" ]
            [ Html.div [ Attrs.class "container" ]
                [ Html.h1
                    []
                    [ Html.text "Inertial Scroll Detection" ]
                , Html.p
                    []
                    [ Html.text "Below is a scrollable box. On your touch device, scroll around by "
                    , Html.em [] [ Html.text "flicking" ]
                    , Html.text " the content to make it inertially scroll."
                    ]
                , Html.div [ Attrs.class "columns is-mobile" ]
                    [ Html.div [ Attrs.class "column" ]
                        [ Html.div [ Attrs.class "has-text-weight-bold" ] [ Html.text "Horiz" ]
                        , Html.div [] [ Html.text "scrollLeft: ", Html.text (String.fromInt (Detector.scrollLeft model.scrollState)) ]
                        , Html.div [] [ Html.text "inertialX: ", Html.text (Detector.inertialX model.scrollState |> directionToString) ]
                        , Html.div [] [ Html.text "stickyX: ", Html.text (Detector.stickyX model.scrollState |> directionToString) ]
                        ]
                    , Html.div [ Attrs.class "column" ]
                        [ Html.div [ Attrs.class "has-text-weight-bold" ] [ Html.text "Vert" ]
                        , Html.div [] [ Html.text "scrollTop: ", Html.text (String.fromInt (Detector.scrollTop model.scrollState)) ]
                        , Html.div [] [ Html.text "inertialY: ", Html.text (Detector.inertialY model.scrollState |> directionToString) ]
                        , Html.div [] [ Html.text "stickyY: ", Html.text (Detector.stickyY model.scrollState |> directionToString) ]
                        ]
                    ]
                , Html.div
                    [ Attrs.class "columns is-centered" ]
                    [ Html.div [ Attrs.class "column is-half" ]
                        [ Html.div
                            ([ Attrs.class "is-scrollable content" ]
                                ++ Detector.onInertialScroll Scrolled
                            )
                            (List.repeat 100 (Html.p [] [ Html.text longText ]))
                        ]
                    ]
                ]
            ]
        ]


longText : String
longText =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."


style : String
style =
    """
html, body {
  font-size: 18px;
}

.is-scrollable {
  border: 1px solid rgba(0 0 0 / 0.3);
  height: 40vh;
  overflow: auto;
  white-space: nowrap;
  user-select: none;
  cursor: pointer;
}
"""
