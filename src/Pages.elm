module Pages exposing (groupExercisesByDay, viewDaysList, viewPage)

import Bootstrap exposing (col, row)
import Date exposing (Date)
import Dict exposing (Dict)
import Dict.Extra exposing (groupBy)
import Helpers
import Html exposing (Html)
import Html.Attributes
import Model.ExerciseVersion2 exposing (Exercise)
import Router
import Words exposing (strings)


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


groupExercisesByDay : List Exercise -> Dict Int (List Exercise)
groupExercisesByDay exercises =
    Dict.Extra.groupBy (\exercise -> Date.toRataDie exercise.date) exercises


viewDaysList : List ( Date, List Exercise ) -> String -> List (Html msg) -> Html msg
viewDaysList days header buttons =
    viewPage
        header
        Nothing
        (Html.div
            []
            (List.append
                [ row []
                    [ Html.div []
                        buttons
                        |> col []
                    ]
                ]
                (if List.isEmpty days then
                    [ Html.div [ Html.Attributes.class "d-flex justify-content-center mt-3" ] [ Html.text strings.noExercises ] ]

                 else
                    List.map viewDayLink days
                )
            )
        )


viewDayLink : ( Date, List Exercise ) -> Html msg
viewDayLink ( date, exercises ) =
    let
        dateStr =
            Helpers.dateToLongString date

        exercisesLength =
            List.length exercises

        doneNumber =
            List.length (List.filter (\e -> e.validated) exercises)
    in
    row [ Html.Attributes.class "my-3" ]
        [ Html.a
            [ Html.Attributes.href (Router.ShowDay date |> Router.toLink), Html.Attributes.class "dayLink" ]
            [ Html.span []
                [ Html.h3
                    [ Html.Attributes.class "d-inline mr-3" ]
                    [ Html.text dateStr ]
                , Html.br [] []
                , Html.span
                    [ Html.Attributes.class "text-muted" ]
                    [ Html.text (strings.numberOfExercisesInDay doneNumber exercisesLength)
                    ]
                ]
                |> col []
            ]
        ]
