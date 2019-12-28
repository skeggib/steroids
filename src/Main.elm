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
    | CreateExercise CreateExerciseForm.Form
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map ListExercises top
        , map ListExercises (s "exercises")
        , map (CreateExercise CreateExerciseForm.init) (s "exercises" </> s "create")
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
    | CreateExerciseMsg CreateExerciseForm.Msg
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

        CreateExerciseMsg formMsg ->
            case model.route of
                CreateExercise form ->
                    let
                        newRoute =
                            CreateExercise (CreateExerciseForm.update formMsg form)
                    in
                    case ( formMsg, CreateExerciseForm.getOutput form ) of
                        ( Form.Submit, Just exercise ) ->
                            ( { model | exercises = exercise :: model.exercises }, Nav.pushUrl model.key "/exercises" )

                        _ ->
                            ( { model | route = newRoute }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

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
