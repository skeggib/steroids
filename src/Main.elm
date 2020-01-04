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
import Json.Decode
import Json.Encode
import Random
import Route exposing (Route(..), parseRoute)
import Storage exposing (Store)
import Task
import Time
import Url


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
    , seed : LoadingValue String Random.Seed
    , store : LoadingValue String Store
    }


type LoadingValue error value
    = LoadingValue
    | LoadedValue value
    | LoadingError error


type alias LoadedModel =
    { key : Nav.Key
    , url : Url.Url
    , store : Store
    , route : Route
    , seed : Random.Seed
    }


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Loading { key = key, url = url, seed = LoadingValue, store = LoadingValue }
    , Cmd.batch
        [ requestTimeForSeed
        , requestStorage
        ]
    )


requestTimeForSeed : Cmd Msg
requestTimeForSeed =
    Task.perform CreateSeed Time.now


requestStorage : Cmd Msg
requestStorage =
    Storage.request ()



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | CreateSeed Time.Posix
    | ReceiveStore Json.Encode.Value
    | CreateExerciseMsg CreateExerciseForm.Msg
    | CancelCreateExercise


updateLoading : Msg -> LoadingModel -> ( Model, Cmd Msg )
updateLoading msg model =
    case msg of
        UrlRequested urlRequest ->
            updateUrlRequested urlRequest (Loading model) model.key

        UrlChanged url ->
            ( Loading { model | url = url }, Cmd.none )

        CreateSeed time ->
            let
                updatedModel =
                    { model | seed = LoadedValue (Random.initialSeed (Time.posixToMillis time)) }

                maybeLoaded =
                    loadingToLoaded updatedModel
            in
            case maybeLoaded of
                Just loaded ->
                    ( Loaded loaded
                    , Nav.pushUrl updatedModel.key (Url.toString updatedModel.url)
                    )

                Nothing ->
                    ( Loading updatedModel, Cmd.none )

        ReceiveStore jsonValue ->
            let
                resultStore =
                    Json.Decode.decodeValue Storage.decoder jsonValue

                resultNull =
                    Json.Decode.decodeValue (Json.Decode.null True) jsonValue

                updateAndTryLoading store =
                    let
                        updatedModel =
                            { model | store = LoadedValue store }

                        maybeLoaded =
                            loadingToLoaded updatedModel
                    in
                    case maybeLoaded of
                        Just loaded ->
                            ( Loaded loaded
                            , Nav.pushUrl updatedModel.key (Url.toString updatedModel.url)
                            )

                        Nothing ->
                            ( Loading updatedModel, Cmd.none )
            in
            case ( resultStore, resultNull ) of
                ( Ok store, _ ) ->
                    updateAndTryLoading store

                ( _, Ok _ ) ->
                    updateAndTryLoading Storage.init

                ( Err error, _ ) ->
                    ( Loading { model | store = LoadingError (Json.Decode.errorToString error) }
                    , Cmd.none
                    )

        -- TODO: log error
        CreateExerciseMsg _ ->
            ( Loading model, Cmd.none )

        -- TODO: log error
        CancelCreateExercise ->
            ( Loading model, Cmd.none )


updateLoaded : Msg -> LoadedModel -> ( Model, Cmd Msg )
updateLoaded msg model =
    case msg of
        UrlRequested urlRequest ->
            updateUrlRequested urlRequest (Loaded model) model.key

        UrlChanged url ->
            let
                route =
                    parseRoute url
            in
            case route of
                DeleteExercise id ->
                    let
                        existingExercises =
                            Storage.getExercises model.store

                        filteredExercises =
                            List.filter (\exercise -> exercise.id /= id) existingExercises

                        newStore =
                            Storage.setExercises filteredExercises model.store
                    in
                    ( Loaded
                        { model
                            | url = url
                            , route = route
                            , store = newStore
                        }
                    , Cmd.batch
                        [ Storage.save newStore
                        , Nav.pushUrl model.key "/exercises"
                        ]
                    )

                _ ->
                    ( Loaded { model | url = url, route = route }, Cmd.none )

        CreateExerciseMsg formMsg ->
            case model.route of
                CreateExercise form ->
                    let
                        newRoute =
                            CreateExercise (CreateExerciseForm.update formMsg form)
                    in
                    case ( formMsg, CreateExerciseForm.getOutput form model.seed ) of
                        ( Form.Submit, Just tuple ) ->
                            let
                                newStore =
                                    Storage.setExercises (Tuple.first tuple :: Storage.getExercises model.store) model.store
                            in
                            ( Loaded
                                { model
                                    | store = newStore
                                    , seed = Tuple.second tuple
                                }
                            , Cmd.batch
                                [ Storage.save newStore
                                , Nav.pushUrl model.key "/exercises"
                                ]
                            )

                        _ ->
                            ( Loaded { model | route = newRoute }, Cmd.none )

                _ ->
                    ( Loaded model, Cmd.none )

        CancelCreateExercise ->
            ( Loaded model, Nav.pushUrl model.key "/exercises" )

        -- TODO: log error
        CreateSeed _ ->
            ( Loaded model, Cmd.none )

        ReceiveStore jsonValue ->
            let
                resultStore =
                    Json.Decode.decodeValue Storage.decoder jsonValue
            in
            case resultStore of
                Ok store ->
                    ( Loaded { model | store = store }
                    , Cmd.none
                    )

                Err error ->
                    let
                        -- TODO: log error
                        _ =
                            Debug.log (Json.Decode.errorToString error)
                    in
                    ( Loaded model, Cmd.none )


updateUrlRequested : Browser.UrlRequest -> Model -> Nav.Key -> ( Model, Cmd msg )
updateUrlRequested urlRequest model key =
    case urlRequest of
        Browser.Internal url ->
            ( model, Nav.pushUrl key (Url.toString url) )

        Browser.External href ->
            ( model, Nav.load href )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading loading ->
            updateLoading msg loading

        Loaded loaded ->
            updateLoaded msg loaded


loadingToLoaded : LoadingModel -> Maybe LoadedModel
loadingToLoaded loading =
    let
        maybeSeed =
            loading.seed

        maybeStore =
            loading.store
    in
    case ( maybeSeed, maybeStore ) of
        ( LoadedValue seed, LoadedValue store ) ->
            Just
                { key = loading.key
                , url = loading.url
                , store = store
                , route = parseRoute loading.url
                , seed = seed
                }

        _ ->
            Nothing


subscriptions : Model -> Sub Msg
subscriptions _ =
    Storage.receive ReceiveStore



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Steroids"
    , body =
        [ case model of
            Loading loadingModel ->
                viewLoading loadingModel

            Loaded loadedModel ->
                viewLoaded loadedModel
        ]
    }


viewLoading : LoadingModel -> Html.Html Msg
viewLoading model =
    case ( model.seed, model.store ) of
        ( LoadingError error, _ ) ->
            Html.text ("Cannot load seed: " ++ error)

        ( _, LoadingError error ) ->
            Html.text ("Cannot load store: " ++ error)

        ( _, _ ) ->
            Html.text "Loading..."


viewLoaded : LoadedModel -> Html.Html Msg
viewLoaded model =
    case model.route of
        NotFound ->
            viewNotFound

        ListExercises ->
            viewListExercises (Storage.getExercises model.store)

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
