module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Date
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


type alias CreateExerciseModel =
    { name : String
    , nameError : String
    , setsNumber : Maybe Int
    , setsNumberError : String
    , repetitionsNumber : Maybe Int
    , repetitionsNumberError : String
    , date : String
    , dateError : String
    }


newCreateExerciseModel : CreateExerciseModel
newCreateExerciseModel =
    { name = ""
    , nameError = ""
    , setsNumber = Nothing
    , setsNumberError = ""
    , repetitionsNumber = Nothing
    , repetitionsNumberError = ""
    , date = ""
    , dateError = ""
    }


convertCreateExerciseModelToExercise : CreateExerciseModel -> Result CreateExerciseModel Exercise
convertCreateExerciseModelToExercise createExerciseModel =
    let
        nameValid =
            String.length createExerciseModel.name > 0

        setsNumberValid =
            case createExerciseModel.setsNumber of
                Just setsNumber ->
                    setsNumber > 0

                Nothing ->
                    False

        repetitionsNumberValid =
            case createExerciseModel.repetitionsNumber of
                Just repetitionsNumber ->
                    repetitionsNumber > 0

                Nothing ->
                    False

        resultDate =
            Date.fromString createExerciseModel.date

        dateValid =
            case resultDate of
                Ok _ ->
                    True

                Err _ ->
                    False
    in
    if nameValid && setsNumberValid && repetitionsNumberValid && dateValid then
        Ok
            { name = createExerciseModel.name
            , setsNumber =
                case createExerciseModel.setsNumber of
                    Just setsNumber ->
                        setsNumber

                    Nothing ->
                        0
            , repetitionsNumber =
                case createExerciseModel.repetitionsNumber of
                    Just repetitionsNumber ->
                        repetitionsNumber

                    Nothing ->
                        0
            , date =
                case resultDate of
                    Ok date ->
                        date

                    Err _ ->
                        { day = 0, month = 0, year = 0 }
            }

    else
        Err
            { createExerciseModel
                | nameError =
                    if nameValid then
                        ""

                    else
                        "The name cannot be empty"
                , setsNumberError =
                    if setsNumberValid then
                        ""

                    else
                        "The sets number has to be greater than zero"
                , repetitionsNumberError =
                    if repetitionsNumberValid then
                        ""

                    else
                        "The repetitions number has to be greater than zero"
                , dateError =
                    case resultDate of
                        Ok _ ->
                            ""

                        Err dateError ->
                            dateError
            }


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key
        url
        [ { name = "Exercise 1", setsNumber = 10, repetitionsNumber = 5, date = { day = 17, month = 3, year = 2018 } }
        , { name = "Exercise 2", setsNumber = 2, repetitionsNumber = 20, date = { day = 19, month = 3, year = 2018 } }
        ]
        (parseRoute url)
    , Cmd.none
    )



-- ROUTE


type Route
    = ListExercises
    | CreateExercice CreateExerciseModel
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map ListExercises top
        , map ListExercises (s "exercises")
        , map (CreateExercice newCreateExerciseModel) (s "exercises" </> s "create")
        ]


parseRoute : Url.Url -> Route
parseRoute url =
    case parse routeParser url of
        Nothing ->
            NotFound

        Just route ->
            route



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | ValidateCreateExercise
    | CancelCreateExercise
    | UpdateCreateExerciceName String
    | UpdateCreateExerciceSetsNumber String
    | UpdateCreateExerciceRepetitionsNumber String
    | UpdateCreateExerciceDate String


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

        ValidateCreateExercise ->
            case model.route of
                CreateExercice createExerciseModel ->
                    case convertCreateExerciseModelToExercise createExerciseModel of
                        Ok exercice ->
                            ( { model | exercises = exercice :: model.exercises }, Nav.pushUrl model.key "/exercises" )

                        Err createExerciseModelWithErrors ->
                            ( { model | route = CreateExercice createExerciseModelWithErrors }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CancelCreateExercise ->
            ( model, Nav.pushUrl model.key "/exercises" )

        UpdateCreateExerciceName newName ->
            case model.route of
                CreateExercice createExerciseModel ->
                    ( { model | route = CreateExercice { createExerciseModel | name = newName } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateCreateExerciceSetsNumber newSetsNumberStr ->
            case model.route of
                CreateExercice createExerciseModel ->
                    ( { model | route = CreateExercice { createExerciseModel | setsNumber = String.toInt newSetsNumberStr } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateCreateExerciceRepetitionsNumber newRepetitionsNumberStr ->
            case model.route of
                CreateExercice createExerciseModel ->
                    ( { model | route = CreateExercice { createExerciseModel | repetitionsNumber = String.toInt newRepetitionsNumberStr } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateCreateExerciceDate newDate ->
            case model.route of
                CreateExercice createExerciseModel ->
                    ( { model | route = CreateExercice { createExerciseModel | date = newDate } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


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

            CreateExercice createExerciseModel ->
                viewCreateExercise createExerciseModel
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


viewCreateExercise : CreateExerciseModel -> Html.Html Msg
viewCreateExercise createExerciseModel =
    Html.div []
        [ Html.div [] [ Html.text "Create an exercice" ]
        , Html.div [] [ Html.input [ Html.Events.onInput UpdateCreateExerciceName, Html.Attributes.placeholder "Exercise name" ] [], Html.text createExerciseModel.nameError ]
        , Html.div [] [ Html.input [ Html.Events.onInput UpdateCreateExerciceSetsNumber, Html.Attributes.placeholder "Sets number" ] [], Html.text createExerciseModel.setsNumberError ]
        , Html.div [] [ Html.input [ Html.Events.onInput UpdateCreateExerciceRepetitionsNumber, Html.Attributes.placeholder "Repetions number" ] [], Html.text createExerciseModel.repetitionsNumberError ]
        , Html.div [] [ Html.input [ Html.Events.onInput UpdateCreateExerciceDate, Html.Attributes.placeholder "Date" ] [], Html.text createExerciseModel.dateError ]
        , Html.div [] [ Html.button [ Html.Events.onClick ValidateCreateExercise ] [ Html.text "Create" ] ]
        , Html.div [] [ Html.button [ Html.Events.onClick CancelCreateExercise ] [ Html.text "Cancel" ] ]
        ]
