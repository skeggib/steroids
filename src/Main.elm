module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Date
import Form
import Form.Error
import Form.Field
import Form.Input
import Form.Validate
import Html
import Html.Attributes
import Html.Events
import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, top)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , exercises : List Exercise
    , route : Route
    }


type alias Exercise =
    { name : String
    , setsNumber : Int
    , repetitionsNumber : Int
    , date : Date.Date
    }


type ExerciseError
    = InvalidDate String


validateExercise : Form.Validate.Validation ExerciseError Exercise
validateExercise =
    Form.Validate.succeed Exercise
        |> Form.Validate.andMap (Form.Validate.field "name" Form.Validate.string)
        |> Form.Validate.andMap
            (Form.Validate.field "setsNumber"
                (Form.Validate.int
                    |> Form.Validate.andThen (Form.Validate.minInt 1)
                )
            )
        |> Form.Validate.andMap
            (Form.Validate.field "repetitionsNumber"
                (Form.Validate.int
                    |> Form.Validate.andThen (Form.Validate.minInt 1)
                )
            )
        |> Form.Validate.andMap (Form.Validate.field "date" validateDate)


validateDate : Form.Validate.Validation ExerciseError Date.Date
validateDate field =
    case Form.Field.asString field of
        Just dateStr ->
            if String.length dateStr == 0 then
                Err (Form.Error.value Form.Error.Empty)

            else
                case Date.fromString dateStr of
                    Ok date ->
                        Ok date

                    Err error ->
                        Err (Form.Error.value (Form.Error.CustomError (InvalidDate error)))
        Nothing ->
            Err (Form.Error.value Form.Error.Empty)


type alias CreateExerciseForm =
    Form.Form ExerciseError Exercise


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key
        url
        []
        (parseRoute url)
    , Cmd.none
    )



-- ROUTE


type Route
    = ListExercises
    | CreateExercise CreateExerciseForm
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map ListExercises top
        , map ListExercises (s "exercises")
        , map (CreateExercise (Form.initial [] validateExercise)) (s "exercises" </> s "create")
        ]


parseRoute : Url.Url -> Route
parseRoute url =
    case parse routeParser url of
        Just route ->
            route

        Nothing ->
            NotFound



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | CreateExerciseFormMsg Form.Msg
    | CancelCreateExercise


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url, route = parseRoute url }
            , Cmd.none
            )

        CreateExerciseFormMsg formMsg ->
            case model.route of
                CreateExercise form ->
                    let
                        newRoute = CreateExercise (Form.update validateExercise formMsg form)
                    in
                        case (formMsg, Form.getOutput form) of
                            (Form.Submit, Just exercise) ->
                                ( { model | exercises = exercise :: model.exercises }, Nav.pushUrl model.key "/exercises" )
                            _ ->
                                ({ model | route = newRoute }, Cmd.none)
                _ ->
                    (model, Cmd.none)                

        CancelCreateExercise ->
            ( model, Nav.pushUrl model.key "/exercises" )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Steroids"
    , body =
        [ case model.route of
            NotFound ->
                viewNotFound

            ListExercises ->
                viewListExercises model.exercises

            CreateExercise form ->
                viewCreateExercise form
        ]
    }


viewNotFound : Html.Html Msg
viewNotFound =
    Html.text "Not found"


viewExercise : Exercise -> Html.Html Msg
viewExercise exercise =
    Html.div []
        [ Html.text (exercise.name ++ " " ++ Date.toString exercise.date ++ " (" ++ String.fromInt exercise.setsNumber ++ " sets, " ++ String.fromInt exercise.repetitionsNumber ++ " repetitions)")
        ]


viewListExercises : List Exercise -> Html.Html Msg
viewListExercises exercises =
    Html.div []
        (List.append
            [ Html.div [] [ Html.text "Exercises list" ]
            , Html.div [] [ Html.a [ Html.Attributes.href "/exercises/create" ] [ Html.text "Create an exercise" ] ]
            ]
            (List.map viewExercise exercises)
        )


viewCreateExercise : CreateExerciseForm -> Html.Html Msg
viewCreateExercise form =
    Html.div []
        [ Html.map CreateExerciseFormMsg (viewExerciseForm form)
        , Html.div []
            [ Html.button
                [ Html.Events.onClick CancelCreateExercise ]
                [ Html.text "Cancel" ]
            ]
        ]


viewExerciseForm : CreateExerciseForm -> Html.Html Form.Msg
viewExerciseForm form =
    let
        errorFor field =
            case field.liveError of
                Just error ->
                    -- TODO: replace toString with your own translations
                    Html.text (Debug.toString error)

                Nothing ->
                    Html.text ""

        nameField =
            Form.getFieldAsString "name" form

        setsNumberField =
            Form.getFieldAsString "setsNumber" form

        repetitionsNumberField =
            Form.getFieldAsString "repetitionsNumber" form

        dateField =
            Form.getFieldAsString "date" form
    in
    Html.div []
        [ Html.div [] [ Html.text "Create an exercise" ]
        , Html.div []
            [ Html.label [] [ Html.text "Name" ]
            , Form.Input.textInput nameField []
            , errorFor nameField
            ]
        , Html.div []
            [ Html.label [] [ Html.text "Sets number" ]
            , Form.Input.textInput setsNumberField []
            , errorFor setsNumberField
            ]
        , Html.div []
            [ Html.label [] [ Html.text "Repetitions numer" ]
            , Form.Input.textInput repetitionsNumberField []
            , errorFor repetitionsNumberField
            ]
        , Html.div []
            [ Html.label [] [ Html.text "Date" ]
            , Form.Input.textInput dateField []
            , errorFor dateField
            ]
        , Html.div []
            [ Html.button
                [ Html.Events.onClick Form.Submit ]
                [ Html.text "Create" ]
            ]
        ]
