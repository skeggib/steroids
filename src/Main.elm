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


type ExerciseNameStatus
    = ValidName String
    | EmptyName


validateName : String -> ExerciseNameStatus
validateName name =
    if String.length name > 0 then
        ValidName name
    else
        EmptyName


type ExerciseSetsNumberStatus
    = ValidSetsNumber Int
    | NotANumberSetsNumber
    | IllegalValueSetsNumber


validateSetsNumber : String -> ExerciseSetsNumberStatus
validateSetsNumber setsNumberStr =
    case String.toInt setsNumberStr of
        Just setsNumber ->
            if setsNumber > 0 then
                ValidSetsNumber setsNumber
            else
                IllegalValueSetsNumber
        Nothing ->
            NotANumberSetsNumber


type ExerciseRepetitionsNumberStatus
    = ValidRepetitionsNumber Int
    | NotANumberRepetitionsNumber
    | IllegalValueRepetitionsNumber


validateRepetitionsNumber : String -> ExerciseRepetitionsNumberStatus
validateRepetitionsNumber repetitionsNumberStr =
    case String.toInt repetitionsNumberStr of
        Just repetitionsNumber ->
            if repetitionsNumber > 0 then
                ValidRepetitionsNumber repetitionsNumber
            else
                IllegalValueRepetitionsNumber
        Nothing ->
            NotANumberRepetitionsNumber


type ExerciseDateStatus
    = ValidDate Date.Date
    | EmptyDate
    | CannotParseDate String


validateDate : String -> ExerciseDateStatus
validateDate dateStr =
    if String.length dateStr == 0 then
        EmptyDate
    else
        case Date.fromString dateStr of
            Ok date ->
                ValidDate date
            Err error ->
                CannotParseDate error



type alias CreateExerciseModel =
    { name : String
    , nameStatus : ExerciseNameStatus
    , setsNumber : String
    , setsNumberStatus : ExerciseSetsNumberStatus
    , repetitionsNumber : String
    , repetitionsNumberStatus : ExerciseRepetitionsNumberStatus
    , date : String
    , dateStatus : ExerciseDateStatus
    }


newCreateExerciseModel : CreateExerciseModel
newCreateExerciseModel =
    { name = ""
    , nameStatus = validateName ""
    , setsNumber = ""
    , setsNumberStatus = validateSetsNumber ""
    , repetitionsNumber = ""
    , repetitionsNumberStatus = validateRepetitionsNumber ""
    , date = ""
    , dateStatus = validateDate ""
    }


convertCreateExerciseModelToExercise : CreateExerciseModel -> Result CreateExerciseModel Exercise
convertCreateExerciseModelToExercise createExerciseModel =
    let
        nameStatus = validateName createExerciseModel.name

        setsNumberStatus = validateSetsNumber createExerciseModel.setsNumber

        repetitionsNumberStatus = validateRepetitionsNumber createExerciseModel.repetitionsNumber

        dateStatus = validateDate createExerciseModel.date
    in
        case nameStatus of
            ValidName name ->
                case setsNumberStatus of
                    ValidSetsNumber setsNumber ->
                        case repetitionsNumberStatus of
                            ValidRepetitionsNumber repetitionsNumber ->
                                case dateStatus of
                                    ValidDate date ->
                                        Ok
                                            { name = name
                                            , setsNumber = setsNumber
                                            , repetitionsNumber = repetitionsNumber
                                            , date = date
                                            }
                                    _ ->
                                        Err { createExerciseModel | nameStatus = nameStatus, setsNumberStatus = setsNumberStatus, repetitionsNumberStatus = repetitionsNumberStatus, dateStatus = dateStatus }
                            _ ->
                                Err { createExerciseModel | nameStatus = nameStatus, setsNumberStatus = setsNumberStatus, repetitionsNumberStatus = repetitionsNumberStatus, dateStatus = dateStatus }
                    _ ->
                        Err { createExerciseModel | nameStatus = nameStatus, setsNumberStatus = setsNumberStatus, repetitionsNumberStatus = repetitionsNumberStatus, dateStatus = dateStatus }
            _ ->
                Err { createExerciseModel | nameStatus = nameStatus, setsNumberStatus = setsNumberStatus, repetitionsNumberStatus = repetitionsNumberStatus, dateStatus = dateStatus }


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

        UpdateCreateExerciceSetsNumber newSetsNumber ->
            case model.route of
                CreateExercice createExerciseModel ->
                    ( { model | route = CreateExercice { createExerciseModel | setsNumber = newSetsNumber } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateCreateExerciceRepetitionsNumber newRepetitionsNumber ->
            case model.route of
                CreateExercice createExerciseModel ->
                    ( { model | route = CreateExercice { createExerciseModel | repetitionsNumber = newRepetitionsNumber } }, Cmd.none )

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


nameStatusToErrorMessage : ExerciseNameStatus -> String
nameStatusToErrorMessage status =
    case status of
        ValidName _ -> ""
        EmptyName -> "Please enter the name"


setsNumberStatusToErrorMessage : ExerciseSetsNumberStatus -> String
setsNumberStatusToErrorMessage status =
    case status of
        ValidSetsNumber _ -> ""
        NotANumberSetsNumber -> "Please enter a sets number"
        IllegalValueSetsNumber -> "The sets number must be greater than zero"


repetitionsNumberStatusToErrorMessage : ExerciseRepetitionsNumberStatus -> String
repetitionsNumberStatusToErrorMessage status =
    case status of
        ValidRepetitionsNumber _ -> ""
        NotANumberRepetitionsNumber -> "Please enter a repetitions number"
        IllegalValueRepetitionsNumber -> "The repetitions number must be greater than zero"


dateStatusToErrorMessage : ExerciseDateStatus -> String
dateStatusToErrorMessage status =
    case status of
        ValidDate _ -> ""
        EmptyDate -> "Please enter the date"
        CannotParseDate error -> error



viewCreateExercise : CreateExerciseModel -> Html.Html Msg
viewCreateExercise createExerciseModel =
    Html.div []
        [ Html.div [] [ Html.text "Create an exercice" ]
        , Html.div [] 
            [ Html.input [ Html.Events.onInput UpdateCreateExerciceName, Html.Attributes.placeholder "Exercise name" ] []
            , Html.text (nameStatusToErrorMessage createExerciseModel.nameStatus)
            ]
        , Html.div []
            [ Html.input [ Html.Events.onInput UpdateCreateExerciceSetsNumber, Html.Attributes.placeholder "Sets number" ] []
            , Html.text (setsNumberStatusToErrorMessage createExerciseModel.setsNumberStatus)
            ]
        , Html.div []
            [ Html.input [ Html.Events.onInput UpdateCreateExerciceRepetitionsNumber, Html.Attributes.placeholder "Repetions number" ] []
            , Html.text (repetitionsNumberStatusToErrorMessage createExerciseModel.repetitionsNumberStatus)
            ]
        , Html.div []
            [ Html.input [ Html.Events.onInput UpdateCreateExerciceDate, Html.Attributes.placeholder "Date" ] []
            , Html.text (dateStatusToErrorMessage createExerciseModel.dateStatus) ]
        , Html.div [] [ Html.button [ Html.Events.onClick ValidateCreateExercise ] [ Html.text "Create" ] ]
        , Html.div [] [ Html.button [ Html.Events.onClick CancelCreateExercise ] [ Html.text "Cancel" ] ]
        ]
