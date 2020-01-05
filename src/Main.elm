module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import CreateExerciseForm
import Date exposing (Date)
import Dict exposing (Dict)
import Dict.Extra
import Exercise exposing (Exercise)
import Form
import Form.Error
import Html exposing (Html)
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
    , store : LoadingValue String Store
    , seed : LoadingValue Never Random.Seed
    , today : LoadingValue Never Date
    }


type LoadingValue error value
    = LoadingValue
    | LoadedValue value
    | LoadingError error


type alias LoadedModel =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , createExerciseForm : CreateExerciseForm.Form
    , store : Store
    , seed : Random.Seed
    , today : Date
    }


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Loading { key = key, url = url, seed = LoadingValue, store = LoadingValue, today = LoadingValue }
    , Cmd.batch
        [ requestStorage
        , requestTimeForSeed
        , requestToday
        ]
    )


requestStorage : Cmd Msg
requestStorage =
    Storage.request ()


requestTimeForSeed : Cmd Msg
requestTimeForSeed =
    Task.perform CreateSeed Time.now


requestToday : Cmd Msg
requestToday =
    Task.perform ReceiveToday Date.today



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | ReceiveStore Json.Encode.Value
    | CreateSeed Time.Posix
    | ReceiveToday Date
    | CreateExerciseMsg CreateExerciseForm.Msg
    | CancelCreateExercise


updateLoading : Msg -> LoadingModel -> ( Model, Cmd Msg )
updateLoading msg model =
    case msg of
        UrlRequested urlRequest ->
            updateUrlRequested urlRequest (Loading model) model.key

        UrlChanged url ->
            ( Loading { model | url = url }, Cmd.none )

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

        ReceiveToday today ->
            let
                updatedModel =
                    { model | today = LoadedValue today }

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
                        , Nav.pushUrl model.key (Route.toLink Route.ListNextDays)
                        ]
                    )

                CreateExercise ->
                    ( Loaded
                        { model
                            | createExerciseForm = CreateExerciseForm.init
                            , url = url
                            , route = route
                        }
                    , Cmd.none
                    )

                _ ->
                    ( Loaded { model | url = url, route = route }, Cmd.none )

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

        -- TODO: log error
        CreateSeed _ ->
            ( Loaded model, Cmd.none )

        ReceiveToday today ->
            ( Loaded { model | today = today }
            , Cmd.none
            )

        CreateExerciseMsg formMsg ->
            case model.route of
                CreateExercise ->
                    let
                        newForm =
                            CreateExerciseForm.update formMsg model.createExerciseForm
                    in
                    case ( formMsg, CreateExerciseForm.getOutput newForm model.seed ) of
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
                                , Nav.pushUrl model.key (Route.toLink Route.ListNextDays)
                                ]
                            )

                        _ ->
                            ( Loaded { model | createExerciseForm = newForm }, Cmd.none )

                _ ->
                    ( Loaded model, Cmd.none )

        CancelCreateExercise ->
            ( Loaded model, Nav.pushUrl model.key (Route.toLink Route.ListNextDays) )


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
    case ( loading.seed, loading.store, loading.today ) of
        ( LoadedValue seed, LoadedValue store, LoadedValue today ) ->
            Just
                { key = loading.key
                , url = loading.url
                , route = parseRoute loading.url
                , createExerciseForm = CreateExerciseForm.init
                , store = store
                , seed = seed
                , today = today
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


viewLoading : LoadingModel -> Html Msg
viewLoading model =
    case ( model.seed, model.store ) of
        ( LoadingError error, _ ) ->
            Html.text "Cannot load seed"

        ( _, LoadingError error ) ->
            Html.text ("Cannot load store: " ++ error)

        ( _, _ ) ->
            Html.text "Loading..."


viewLoaded : LoadedModel -> Html Msg
viewLoaded model =
    case model.route of
        NotFound ->
            viewNotFound

        ListNextDays ->
            viewNextDays model.today (Storage.getExercises model.store)

        ListPastDays ->
            viewPastDays model.today (Storage.getExercises model.store)

        CreateExercise ->
            viewCreateExercise model.createExerciseForm

        DeleteExercise id ->
            Html.text ("Deleting " ++ Exercise.idToString id ++ "...")

        ShowDay date ->
            viewDay date (Storage.getExercises model.store)


viewNotFound : Html Msg
viewNotFound =
    Html.text "Not found"


groupExercisesByDay : List Exercise -> Dict Int (List Exercise)
groupExercisesByDay exercises =
    Dict.Extra.groupBy (\exercise -> Date.toRataDie exercise.date) exercises


viewNextDays : Date -> List Exercise -> Html Msg
viewNextDays today exercises =
    let
        todayRataDie =
            Date.toRataDie today

        nextExercises =
            List.filter (\exercise -> Date.toRataDie exercise.date >= todayRataDie) exercises

        groups =
            Dict.toList (groupExercisesByDay nextExercises)
    in
    Html.div []
        (List.append
            [ Html.div [] [ Html.text "Exercises" ]
            , Html.div [] [ Html.a [ Html.Attributes.href (Route.toLink Route.CreateExercise) ] [ Html.text "Create an exercise" ] ]
            , Html.div [] [ Html.a [ Html.Attributes.href (Route.toLink Route.ListPastDays) ] [ Html.text "View past exercises" ] ]
            ]
            (List.map viewDayLink groups)
        )


viewPastDays : Date -> List Exercise -> Html Msg
viewPastDays today exercises =
    let
        todayRataDie =
            Date.toRataDie today

        nextExercises =
            List.filter (\exercise -> Date.toRataDie exercise.date < todayRataDie) exercises

        groups =
            List.reverse (Dict.toList (groupExercisesByDay nextExercises))
    in
    Html.div []
        (List.append
            [ Html.div [] [ Html.text "Past exercises" ]
            , Html.a [ Html.Attributes.href (Route.toLink Route.ListNextDays) ] [ Html.text "Go back to exercises list" ]
            ]
            (List.map viewDayLink groups)
        )


viewDayLink : ( Int, List Exercise ) -> Html Msg
viewDayLink ( day, exercises ) =
    let
        date =
            Date.fromRataDie day

        dateStr =
            Date.toIsoString date
    in
    Html.div []
        [ Html.a
            [ Html.Attributes.href (Route.ShowDay date |> Route.toLink) ]
            [ Html.text (dateStr ++ " (" ++ String.fromInt (List.length exercises) ++ ")") ]
        ]


viewExercise : Exercise -> Html Msg
viewExercise exercise =
    Html.div []
        [ Html.text
            (exercise.name
                ++ " ("
                ++ String.fromInt exercise.setsNumber
                ++ " sets, "
                ++ String.fromInt exercise.repetitionsNumber
                ++ " repetitions)"
            )
        , Html.a [ Html.Attributes.href (Route.DeleteExercise exercise.id |> Route.toLink) ] [ Html.text "Delete" ]
        ]


viewCreateExercise : CreateExerciseForm.Form -> Html Msg
viewCreateExercise form =
    Html.div []
        [ Html.map CreateExerciseMsg (CreateExerciseForm.view form createExerciseErrorToString)
        , Html.div []
            [ Html.button
                [ Html.Events.onClick CancelCreateExercise ]
                [ Html.text "Cancel" ]
            ]
        ]


viewDay : Date -> List Exercise -> Html Msg
viewDay date exercises =
    let
        filteredExercises =
            List.filter (\exercise -> exercise.date == date) exercises
    in
    Html.div []
        [ Html.text (Date.toIsoString date)
        , Html.div []
            (if List.length filteredExercises == 0 then
                [ Html.text "No exercises" ]

             else
                List.map viewExercise filteredExercises
            )
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
