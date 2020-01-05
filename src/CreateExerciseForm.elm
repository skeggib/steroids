module CreateExerciseForm exposing (Error(..), Form, Msg, getOutput, init, update, validate, view)

import Date
import Exercise
import Form exposing (getFieldAsString)
import Form.Error exposing (ErrorValue(..), value)
import Form.Field exposing (asString)
import Form.Input exposing (textInput)
import Form.Validate exposing (Validation, andMap, andThen, field, int, minInt, string, succeed)
import Html
import Html.Events
import Random


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


type alias Msg =
    Form.Msg


init : Form
init =
    Form.initial [] validate


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


getOutput : Form -> Random.Seed -> Maybe ( Exercise.Exercise, Random.Seed )
getOutput form seed =
    Maybe.map (formExerciseToExercise seed) (Form.getOutput form)


formExerciseToExercise : Random.Seed -> Exercise -> ( Exercise.Exercise, Random.Seed )
formExerciseToExercise seed formExercise =
    let
        idTuple =
            Exercise.createId seed
    in
    ( { id = Tuple.first idTuple
      , name = formExercise.name
      , setsNumber = formExercise.setsNumber
      , repetitionsNumber = formExercise.repetitionsNumber
      , date = formExercise.date
      }
    , Tuple.second idTuple
    )


view : Form -> (ErrorValue Error -> String) -> Html.Html Msg
view form errorToString =
    let
        errorFor field =
            case field.liveError of
                Just error ->
                    Html.text (errorToString error)

                Nothing ->
                    Html.text ""

        nameField =
            getFieldAsString "name" form

        setsNumberField =
            getFieldAsString "setsNumber" form

        repetitionsNumberField =
            getFieldAsString "repetitionsNumber" form

        dateField =
            getFieldAsString "date" form
    in
    Html.div []
        [ Html.div [] [ Html.text "Create an exercise" ]
        , Html.div []
            [ Html.label [] [ Html.text "Name" ]
            , textInput nameField []
            , errorFor nameField
            ]
        , Html.div []
            [ Html.label [] [ Html.text "Sets number" ]
            , textInput setsNumberField []
            , errorFor setsNumberField
            ]
        , Html.div []
            [ Html.label [] [ Html.text "Repetitions numer" ]
            , textInput repetitionsNumberField []
            , errorFor repetitionsNumberField
            ]
        , Html.div []
            [ Html.label [] [ Html.text "Date" ]
            , textInput dateField []
            , errorFor dateField
            ]
        , Html.div []
            [ Html.button
                [ Html.Events.onClick Form.Submit ]
                [ Html.text "Create" ]
            ]
        ]
