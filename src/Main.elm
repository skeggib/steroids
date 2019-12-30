module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import CreateExerciseForm
import Date
import Exercise exposing (Exercise)
import Form
import Form.Error
import Html
import Html.Attributes
import Html.Events
import Random
import Task
import Time
import Url
import Url.Parser exposing ((</>), Parser, custom, map, oneOf, parse, s, top)


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


type Model
    = Loading LoadingModel
    | Loaded LoadedModel


type alias LoadingModel =
    { key : Nav.Key
    , url : Url.Url
    }


type alias LoadedModel =
    { key : Nav.Key
    , url : Url.Url
    , exercises : List Exercise
    , route : Route
    , seed : Random.Seed
    }


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Loading { key = key, url = url }
    , Task.perform CreateSeed Time.now
    )



-- ROUTE


type Route
    = ListExercises
    | CreateExercise CreateExerciseForm.Form
    | DeleteExercise Exercise.Id
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map ListExercises top
        , map ListExercises (s "exercises")
        , map (CreateExercise CreateExerciseForm.init) (s "exercises" </> s "create")
        , map DeleteExercise (s "exercises" </> s "delete" </> custom "exerciseId" Exercise.idFromString)
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
    | CreateSeed Time.Posix
    | CreateExerciseMsg CreateExerciseForm.Msg
    | CancelCreateExercise


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case model of
                Loading loading ->
                    case urlRequest of
                        Browser.Internal url ->
                            ( Loading loading, Nav.pushUrl loading.key (Url.toString url) )

                        Browser.External href ->
                            ( Loading loading, Nav.load href )

                Loaded loaded ->
                    case urlRequest of
                        Browser.Internal url ->
                            ( Loaded loaded, Nav.pushUrl loaded.key (Url.toString url) )

                        Browser.External href ->
                            ( Loaded loaded, Nav.load href )

        UrlChanged url ->
            case model of
                Loading loading ->
                    ( Loading { loading | url = url }, Cmd.none )

                Loaded loaded ->
                    let
                        route =
                            parseRoute url
                    in
                    case route of
                        DeleteExercise id ->
                            ( Loaded
                                { loaded
                                    | url = url
                                    , route = route
                                    , exercises = List.filter (\exercise -> exercise.id /= id) loaded.exercises
                                }
                            , Nav.pushUrl loaded.key "/exercises"
                            )

                        _ ->
                            ( Loaded { loaded | url = url, route = route }, Cmd.none )

        CreateExerciseMsg formMsg ->
            case model of
                Loading loading ->
                    ( Loading loading, Cmd.none )

                Loaded loaded ->
                    case loaded.route of
                        CreateExercise form ->
                            let
                                newRoute =
                                    CreateExercise (CreateExerciseForm.update formMsg form)
                            in
                            case ( formMsg, CreateExerciseForm.getOutput form loaded.seed ) of
                                ( Form.Submit, Just tuple ) ->
                                    ( Loaded
                                        { loaded
                                            | exercises = Tuple.first tuple :: loaded.exercises
                                            , seed = Tuple.second tuple
                                        }
                                    , Nav.pushUrl loaded.key "/exercises"
                                    )

                                _ ->
                                    ( Loaded { loaded | route = newRoute }, Cmd.none )

                        _ ->
                            ( Loaded loaded, Cmd.none )

        CancelCreateExercise ->
            case model of
                Loading loading ->
                    ( Loading loading, Cmd.none )

                Loaded loaded ->
                    ( Loaded loaded, Nav.pushUrl loaded.key "/exercises" )

        CreateSeed time ->
            case model of
                Loading loading ->
                    ( Loaded
                        { key = loading.key
                        , url = loading.url
                        , exercises = []
                        , route = parseRoute loading.url
                        , seed = Random.initialSeed (Time.posixToMillis time)
                        }
                    , Nav.pushUrl loading.key (Url.toString loading.url)
                    )

                Loaded loaded ->
                    ( Loaded loaded, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Steroids"
    , body =
        [ case model of
            Loading _ ->
                Html.text "Loading..."

            Loaded loadedModel ->
                viewLoaded loadedModel
        ]
    }


viewLoaded : LoadedModel -> Html.Html Msg
viewLoaded model =
    case model.route of
        NotFound ->
            viewNotFound

        ListExercises ->
            viewListExercises model.exercises

        CreateExercise form ->
            viewCreateExercise form

        DeleteExercise id ->
            Html.text ("Deleting " ++ Exercise.idToString id ++ "...")


viewNotFound : Html.Html Msg
viewNotFound =
    Html.text "Not found"


viewExercise : Exercise -> Html.Html Msg
viewExercise exercise =
    Html.div []
        [ Html.text
            (exercise.name
                ++ " "
                ++ Date.toString exercise.date
                ++ " ("
                ++ String.fromInt exercise.setsNumber
                ++ " sets, "
                ++ String.fromInt exercise.repetitionsNumber
                ++ " repetitions)"
            )
        , Html.a [ Html.Attributes.href ("exercises/delete/" ++ Exercise.idToString exercise.id) ] [ Html.text "Delete" ]
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


viewCreateExercise : CreateExerciseForm.Form -> Html.Html Msg
viewCreateExercise form =
    Html.div []
        [ Html.map CreateExerciseMsg (CreateExerciseForm.view form createExerciseErrorToString)
        , Html.div []
            [ Html.button
                [ Html.Events.onClick CancelCreateExercise ]
                [ Html.text "Cancel" ]
            ]
        ]


createExerciseErrorToString : Form.Error.ErrorValue CreateExerciseForm.Error -> String
createExerciseErrorToString error =
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
                CreateExerciseForm.InvalidDate value ->
                    value
