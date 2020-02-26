module Pages.NextDaysPage exposing (Page, init, view)

import Bootstrap exposing (ButtonStyle(..), buttonHyperlink)
import Date exposing (Date)
import Dict
import Html exposing (Html)
import Html.Attributes
import Model.StorageVersion2 as Storage
import Pages
import Router
import Words exposing (strings)


type alias Page =
    { today : Date
    }


init : Date -> Page
init today =
    { today = today
    }


view : Storage.Store -> Page -> Html msg
view store page =
    let
        todayRataDie =
            Date.toRataDie page.today

        days =
            Storage.getExercises store
                |> List.filter (\exercise -> Date.toRataDie exercise.date >= todayRataDie)
                |> Pages.groupExercisesByDay
                |> Dict.toList
                |> List.sortBy (\( ratadie, _ ) -> ratadie)
                |> List.map (\( ratadie, exercisesList ) -> ( Date.fromRataDie ratadie, exercisesList ))

        buttons =
            [ buttonHyperlink
                Primary
                [ Html.Attributes.class "float-right ml-2" ]
                (Router.toLink Router.CreateExercise)
                strings.actionCreateExercise
            , buttonHyperlink
                Light
                [ Html.Attributes.class "float-right" ]
                (Router.toLink Router.ListPastDays)
                strings.actionViewPastExercises
            ]
    in
    Pages.viewDaysList days strings.titleNextDaysPage buttons
