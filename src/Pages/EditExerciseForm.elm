module Pages.EditExerciseForm exposing (Form, Msg(..), getOutput, init, update, view)

import Bootstrap exposing (col, row)
import Date
import Form exposing (getFieldAsString)
import Form.Error exposing (ErrorValue(..), value)
import Form.Field exposing (asString)
import Form.Input exposing (textInput)
import Form.Validate exposing (Validation, andMap, andThen, field, int, minInt, string, succeed)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Model.ExerciseVersion2 as ExerciseModel


type alias Form =
    Form.Form Error Exercise


type alias Exercise =
    { name : String
    , setsNumber : Int
    , repetitionsNumber : Int
    , date : Date.Date
    }


type Error
    = InvalidDate String


type Msg
    = FormMsg Form.Msg
    | Cancel


init : ExerciseModel.Exercise -> Form
init exercise =
    Form.initial
        [ ( "name", Form.Field.string exercise.name )
        , ( "setsNumber", Form.Field.string (String.fromInt exercise.setsNumber) )
        , ( "repetitionsNumber", Form.Field.string (String.fromInt exercise.repetitionsNumber) )
        , ( "date", Form.Field.string (Date.toIsoString exercise.date) )
        ]
        validate


update : Form.Msg -> Form -> Form
update msg form =
    Form.update validate msg form


validate : Validation Error Exercise
validate =
    succeed Exercise
        |> andMap (field "name" string)
        |> andMap
            (field "setsNumber"
                (int
                    |> andThen (minInt 1)
                )
            )
        |> andMap
            (field "repetitionsNumber"
                (int
                    |> andThen (minInt 1)
                )
            )
        |> andMap (field "date" validateDate)


validateDate : Validation Error Date.Date
validateDate field =
    case asString field of
        Just dateStr ->
            if String.length dateStr == 0 then
                Err (value Empty)

            else
                case Date.fromIsoString dateStr of
                    Ok date ->
                        Ok date

                    Err error ->
                        Err (value (CustomError (InvalidDate error)))

        Nothing ->
            Err (value Empty)


getOutput : Form -> ExerciseModel.Exercise -> Maybe ExerciseModel.Exercise
getOutput form exercise =
    Maybe.map (formExerciseToExercise exercise) (Form.getOutput form)


formExerciseToExercise : ExerciseModel.Exercise -> Exercise -> ExerciseModel.Exercise
formExerciseToExercise exercise formExercise =
    { id = exercise.id
    , name = formExercise.name
    , setsNumber = formExercise.setsNumber
    , repetitionsNumber = formExercise.repetitionsNumber
    , date = formExercise.date
    , validated = exercise.validated
    }


view : Form -> Html Msg
view form =
    let
        nameField =
            getFieldAsString "name" form

        setsNumberField =
            getFieldAsString "setsNumber" form

        repetitionsNumberField =
            getFieldAsString "repetitionsNumber" form

        dateField =
            getFieldAsString "date" form
    in
    Html.div [ Html.Attributes.class "container" ]
        [ row [] [ Html.h1 [ Html.Attributes.class "my-3" ] [ Html.text "Edit an exercise" ] |> col [] ]
        , row [] [ inputGroup "Name" nameField |> col [] ]
        , row []
            [ inputGroup "Sets number" setsNumberField |> col []
            , inputGroup "Repetitions number" repetitionsNumberField |> col []
            ]
        , row [] [ inputGroup "Date" dateField |> col [] ]
        , row [ Html.Attributes.class "mt-3" ]
            [ col []
                (Html.div []
                    [ Html.button [ Html.Attributes.class "btn btn-primary float-right ml-2", Html.Events.onClick (FormMsg Form.Submit) ] [ Html.text "Update" ]
                    , Html.button [ Html.Attributes.class "btn btn-secondary float-right", Html.Events.onClick Cancel ] [ Html.text "Cancel" ]
                    ]
                )
            ]
        ]


inputGroup : String -> Form.FieldState Error String -> Html Msg
inputGroup label field =
    let
        isInvalid =
            case field.liveError of
                Just _ ->
                    True

                Nothing ->
                    False

        feedback =
            case field.liveError of
                Just error ->
                    Html.text (errorToString error)

                Nothing ->
                    Html.text ""
    in
    Html.div [ Html.Attributes.class "form-group" ]
        [ Html.label [] [ Html.text label ]
        , Html.map FormMsg
            (textInput field
                [ Html.Attributes.class
                    ("form-control"
                        ++ (if isInvalid then
                                " is-invalid"

                            else
                                ""
                           )
                    )
                ]
            )
        , Html.div [ Html.Attributes.class "invalid-feedback" ] [ feedback ]
        ]


errorToString : Form.Error.ErrorValue Error -> String
errorToString error =
    case error of
        Form.Error.Empty ->
            "Please fill this field"

        Form.Error.InvalidString ->
            "Please fill this field"

        Form.Error.InvalidEmail ->
            "Please enter a valid email"

        Form.Error.InvalidFormat ->
            "This value is not valid"

        Form.Error.InvalidInt ->
            "This value is not valid"

        Form.Error.InvalidFloat ->
            "This value is not valid"

        Form.Error.InvalidBool ->
            "This value is not valid"

        Form.Error.SmallerIntThan value ->
            "This field cannot be smaller than " ++ String.fromInt value

        Form.Error.GreaterIntThan value ->
            "This field cannot be greater than " ++ String.fromInt value

        Form.Error.SmallerFloatThan value ->
            "This field cannot be smaller than " ++ String.fromFloat value

        Form.Error.GreaterFloatThan value ->
            "This field cannot be greater than " ++ String.fromFloat value

        Form.Error.ShorterStringThan value ->
            "This field must be at least " ++ String.fromInt value ++ " characters long"

        Form.Error.LongerStringThan value ->
            "This field must be at most " ++ String.fromInt value ++ " characters long"

        Form.Error.NotIncludedIn ->
            "I do not know this value"

        Form.Error.CustomError customError ->
            case customError of
                InvalidDate value ->
                    value
