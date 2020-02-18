module Pages exposing (viewPage)

import Bootstrap exposing (col, row)
import Html exposing (Html)
import Html.Attributes


viewPage : String -> Maybe (List (Html msg)) -> Html msg -> Html msg
viewPage header actionBarContent content =
    let
        actionBar =
            case actionBarContent of
                Just justActionBarContent ->
                    Html.div
                        [ Html.Attributes.class "fixed-top p-3 action-bar" ]
                        justActionBarContent

                Nothing ->
                    Html.div
                        [ Html.Attributes.class "fixed-top p-3 action-bar hidden" ]
                        []
    in
    Html.div [ Html.Attributes.style "min-height" "100vh" ]
        [ Html.div
            [ Html.Attributes.class "container"
            , Html.Attributes.style "min-height" "100vh"

            --, Mouse.onDown (\_ -> ExerciseLongPress Gestures.Reset) TODO
            ]
            [ row [] [ Html.h1 [ Html.Attributes.class "my-4" ] [ Html.text header ] |> col [] ]
            , content
            ]
        , actionBar
        ]
