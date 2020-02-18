module Pages.ShowDayPage exposing (Msg, Page, init, update, view)

import Bootstrap exposing (col, row)
import Date exposing (Date)
import Gestures
import Helpers
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Model.ExerciseVersion2 as Exercise exposing (Exercise)
import Model.StorageVersion2 as Storage
import Pages
import Router
import Words exposing (plural, strings, words)


type alias Page =
    { longPress : Gestures.LongPressModel Exercise
    , date : Date
    , store : Storage.Store
    }


type Msg
    = ToggleValidated Exercise.Id
    | ExerciseLongPress (Gestures.LongPressEvent Exercise)



-- (Storage.getExercises model.store)


init : Date -> Storage.Store -> Page
init date store =
    { longPress = Gestures.initLongPress
    , date = date
    , store = store
    }


update : Msg -> Page -> ( Page, Cmd Msg )
update msg page =
    case msg of
        ToggleValidated exerciseId ->
            let
                exercises =
                    Storage.getExercises page.store

                filteredExercises =
                    List.filter (\e -> e.id == exerciseId) exercises

                updatedExercise =
                    case filteredExercises of
                        exerciseToUpdate :: _ ->
                            Just { exerciseToUpdate | validated = not exerciseToUpdate.validated }

                        [] ->
                            Nothing

                updatedExercisesList =
                    case updatedExercise of
                        Just exercise ->
                            List.map
                                (\e ->
                                    if e.id == exerciseId then
                                        exercise

                                    else
                                        e
                                )
                                exercises

                        Nothing ->
                            exercises

                updatedStore =
                    Storage.setExercises updatedExercisesList page.store
            in
            ( { page | store = updatedStore }, Storage.save updatedStore )

        ExerciseLongPress event ->
            let
                ( newLongPress, cmd ) =
                    Gestures.updateLongPress
                        event
                        page.longPress
                        (\e -> ExerciseLongPress e)
            in
            ( { page | longPress = newLongPress }, cmd )


view : Page -> Html Msg
view page =
    viewDay
        page.date
        (Storage.getExercises page.store |> List.filter (\e -> e.date == page.date))
        page.longPress.pressing
        page.longPress.pressed


viewDay : Date -> List Exercise -> Maybe Exercise -> Maybe Exercise -> Html Msg
viewDay date exercises pressingExercise pressedExercise =
    let
        filteredExercises =
            List.filter (\exercise -> exercise.date == date) exercises

        actionBarContent =
            Maybe.map
                (\justPressedExercise ->
                    [ Html.a [ Html.Attributes.href (Router.toLink (Router.DeleteExercise justPressedExercise.id)) ]
                        [ Html.i [ Html.Attributes.class "material-icons float-right px-2" ]
                            [ Html.text "delete" ]
                        ]
                    , Html.a [ Html.Attributes.href (Router.toLink (Router.EditExercise justPressedExercise.id)) ]
                        [ Html.i [ Html.Attributes.class "material-icons float-right px-2" ]
                            [ Html.text "edit" ]
                        ]
                    ]
                )
                pressedExercise
    in
    Pages.viewPage
        (Helpers.dateToLongString date)
        actionBarContent
        (Html.div
            []
            (if List.length filteredExercises == 0 then
                [ Html.text strings.noExercises ]

             else
                List.map
                    (\e ->
                        let
                            pressing =
                                pressingExercise == Just e

                            pressed =
                                pressedExercise == Just e
                        in
                        viewExercise e pressing pressed
                    )
                    filteredExercises
            )
        )


viewExercise : Exercise -> Bool -> Bool -> Html Msg
viewExercise exercise pressing pressed =
    let
        title =
            Html.h4 [ Html.Attributes.class "mb-0" ] [ Html.text exercise.name ]

        checkBox =
            validatedCheckBox exercise.validated []

        text =
            Html.text
                (String.fromInt exercise.setsNumber
                    ++ " "
                    ++ plural words.set exercise.setsNumber
                    ++ ", "
                    ++ String.fromInt exercise.repetitionsNumber
                    ++ " "
                    ++ plural words.repetition exercise.repetitionsNumber
                )
    in
    Html.div
        [ Html.Attributes.class "p-2"
        , Html.Attributes.style "background-color"
            (case ( pressing, pressed ) of
                ( True, False ) ->
                    "#f5f5f5"

                ( False, True ) ->
                    "#b3e5fc"

                ( True, True ) ->
                    "#81d4fa"

                ( False, False ) ->
                    "rgba(0,0,0,0)"
            )
        ]
        [ row []
            [ col
                [ Html.Events.onClick (ToggleValidated exercise.id)
                , Html.Attributes.class "col-2"
                , Html.Attributes.class "d-flex justify-content-center align-items-center"
                ]
                checkBox
            , col
                (List.append
                    [ Html.Attributes.class "col-8 pl-0 exercise" ]
                    (Gestures.longPress
                        exercise
                        (\e -> ExerciseLongPress e)
                    )
                )
                (Html.div []
                    [ row [] [ col [] title ]
                    , row [] [ col [] text ]
                    ]
                )
            ]
        ]


validatedCheckBox : Bool -> List (Html.Attribute msg) -> Html msg
validatedCheckBox checked attributes =
    Html.i
        (List.append
            [ Html.Attributes.class "material-icons"
            , Html.Attributes.style "color"
                (if checked then
                    "#00c853"

                 else
                    "#cccccc"
                )
            , Html.Attributes.style "font-size" "24px"
            ]
            attributes
        )
        [ if checked then
            Html.text "check_circle"

          else
            Html.text "radio_button_unchecked"
        ]
