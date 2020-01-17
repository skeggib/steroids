module Main exposing (main)

import Bootstrap exposing (col, row)
import Browser
import Browser.Navigation as Nav
import CreateExerciseForm
import Date exposing (Date)
import Dict exposing (Dict)
import Dict.Extra
import ExerciseVersion2 as Exercise exposing (Exercise)
import Form
import Helpers
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import Random
import Route exposing (Route(..), parseRoute)
import StorageVersion2 as Storage exposing (Store)
import Task
import Time
import Url
import Words exposing (plural, words)


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
    | ToggleValidated Exercise.Id


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

        CreateExerciseMsg _ ->
            Debug.log "CreateExerciseMsg should not be called in the loading model" ( Loading model, Cmd.none )

        ToggleValidated _ ->
            Debug.log "ToggleValidated should not be called in the loading model" ( Loading model, Cmd.none )


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
                        _ =
                            Debug.log (Json.Decode.errorToString error)
                    in
                    ( Loaded model, Cmd.none )

        CreateSeed _ ->
            Debug.log "CreateSeed should not be called in the loaded model" ( Loaded model, Cmd.none )

        ReceiveToday today ->
            ( Loaded { model | today = today }
            , Cmd.none
            )

        CreateExerciseMsg createFormMsg ->
            case model.route of
                CreateExercise ->
                    case createFormMsg of
                        CreateExerciseForm.FormMsg formMsg ->
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

                        CreateExerciseForm.Cancel ->
                            ( Loaded model, Nav.pushUrl model.key (Route.toLink Route.ListNextDays) )

                _ ->
                    ( Loaded model, Cmd.none )

        ToggleValidated exerciseId ->
            let
                exercises =
                    Storage.getExercises model.store

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
                    Storage.setExercises updatedExercisesList model.store
            in
            ( Loaded { model | store = updatedStore }, Storage.save updatedStore )


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
        ( LoadingError _, _ ) ->
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

        days =
            exercises
                |> List.filter (\exercise -> Date.toRataDie exercise.date >= todayRataDie)
                |> groupExercisesByDay
                |> Dict.toList
                |> List.sortBy (\( ratadie, _ ) -> ratadie)
                |> List.map (\( ratadie, exercisesList ) -> ( Date.fromRataDie ratadie, exercisesList ))

        buttons =
            [ Html.a
                [ Html.Attributes.href (Route.toLink Route.CreateExercise)
                , Html.Attributes.class "btn btn-primary float-right ml-2"
                ]
                [ Html.text "Create an exercise" ]
            , Html.a
                [ Html.Attributes.href (Route.toLink Route.ListPastDays)
                , Html.Attributes.class "btn btn-light float-right"
                ]
                [ Html.text "View past exercises" ]
            ]
    in
    viewDaysList days "Exercises" buttons


viewPastDays : Date -> List Exercise -> Html Msg
viewPastDays today exercises =
    let
        todayRataDie =
            Date.toRataDie today

        days =
            exercises
                |> List.filter (\exercise -> Date.toRataDie exercise.date < todayRataDie)
                |> groupExercisesByDay
                |> Dict.toList
                |> List.sortBy (\( ratadie, _ ) -> ratadie)
                |> List.reverse
                |> List.map (\( ratadie, exercisesList ) -> ( Date.fromRataDie ratadie, exercisesList ))

        buttons =
            [ Html.a
                [ Html.Attributes.href (Route.toLink Route.ListNextDays)
                , Html.Attributes.class "btn btn-light float-right"
                ]
                [ Html.text "Go back to exercises list" ]
            ]
    in
    viewDaysList days "Past exercises" buttons


viewDaysList : List ( Date, List Exercise ) -> String -> List (Html Msg) -> Html Msg
viewDaysList days header buttons =
    viewPage
        header
        (Html.div
            []
            (List.append
                [ row []
                    [ Html.div []
                        buttons
                        |> col []
                    ]
                ]
                (if List.isEmpty days then
                    [ Html.div [ Html.Attributes.class "d-flex justify-content-center mt-3" ] [ Html.text "No exercises!" ] ]

                 else
                    List.map viewDayLink days
                )
            )
        )


viewPage : String -> Html Msg -> Html Msg
viewPage header content =
    Html.div [ Html.Attributes.class "container" ]
        [ row [] [ Html.h1 [ Html.Attributes.class "my-3" ] [ Html.text header ] |> col [] ]
        , content
        ]


viewDayLink : ( Date, List Exercise ) -> Html Msg
viewDayLink ( date, exercises ) =
    let
        dateStr =
            Helpers.dateToLongString date

        exercisesLength =
            List.length exercises

        doneNumber =
            List.length (List.filter (\e -> e.validated) exercises)
    in
    row [ Html.Attributes.class "my-3" ]
        [ Html.a
            [ Html.Attributes.href (Route.ShowDay date |> Route.toLink), Html.Attributes.class "dayLink" ]
            [ Html.span []
                [ Html.h3
                    [ Html.Attributes.class "d-inline mr-3" ]
                    [ Html.text dateStr ]
                , Html.br [] []
                , Html.span
                    [ Html.Attributes.class "text-muted" ]
                    [ Html.text
                        (String.fromInt exercisesLength
                            ++ " "
                            ++ plural words.exercise exercisesLength
                            ++ " ("
                            ++ String.fromInt doneNumber
                            ++ "/"
                            ++ String.fromInt exercisesLength
                            ++ " done)"
                        )
                    ]
                ]
                |> col []
            ]
        ]


validatedCheckBox : Bool -> List (Html.Attribute Msg) -> Html Msg
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


viewExercise : Exercise -> Html Msg
viewExercise exercise =
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

        buttonDelete =
            Html.a
                [ Html.Attributes.href (Route.DeleteExercise exercise.id |> Route.toLink)
                , Html.Attributes.class "btn btn-danger"
                ]
                [ Html.text "Delete" ]
    in
    Html.div []
        [ row []
            [ col
                [ Html.Events.onClick (ToggleValidated exercise.id)
                , Html.Attributes.class "col-3"
                , Html.Attributes.class "d-flex justify-content-center align-items-center"
                ]
                checkBox
            , col [ Html.Attributes.class "col-7" ]
                (row []
                    [ row [] [ col [] title ]
                    , row [] [ col [] text ]
                    ]
                )
            ]
        , row []
            [ col [] buttonDelete
            ]
        ]


viewCreateExercise : CreateExerciseForm.Form -> Html Msg
viewCreateExercise form =
    Html.map CreateExerciseMsg (CreateExerciseForm.view form)


viewDay : Date -> List Exercise -> Html Msg
viewDay date exercises =
    let
        filteredExercises =
            List.filter (\exercise -> exercise.date == date) exercises
    in
    viewPage (Helpers.dateToLongString date)
        (Html.div
            []
            (if List.length filteredExercises == 0 then
                [ Html.text "No exercises!" ]

             else
                List.map viewExercise filteredExercises
            )
        )
