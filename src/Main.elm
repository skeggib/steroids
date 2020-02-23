module Main exposing (main)

import Bootstrap exposing (ButtonStyle(..), buttonHyperlink, col, row)
import Browser
import Browser.Navigation as Nav
import Date exposing (Date)
import Dict exposing (Dict)
import Dict.Extra
import Form
import Gestures
import Helpers
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Json.Decode
import Json.Encode
import Model.ExerciseVersion2 as Exercise exposing (Exercise)
import Model.StorageVersion2 as Storage exposing (Store)
import Pages
import Pages.CreateExerciseForm
import Pages.EditExerciseForm
import Pages.NextDaysPage
import Pages.PastDaysPage
import Pages.ShowDayPage exposing (Msg(..))
import Random
import Router exposing (Route(..), parseRoute)
import Task
import Time
import Url
import Words exposing (plural, strings, words)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \request -> RouterMsg (Router.onUrlRequested request)
        , onUrlChange = \url -> RouterMsg (Router.onUrlChanged url)
        }



-- MODEL


type Model
    = Loading LoadingModel
    | Loaded LoadedModel


type alias LoadingModel =
    { url : Url.Url
    , key : Nav.Key
    , store : LoadingValue String Store
    , seed : LoadingValue Never Random.Seed
    , today : LoadingValue Never Date
    }


type LoadingValue error value
    = LoadingValue
    | LoadedValue value
    | LoadingError error


type alias LoadedModel =
    { router : Router.Router
    , nextDaysPage : Maybe Pages.NextDaysPage.Page
    , pastDaysPage : Maybe Pages.PastDaysPage.Page
    , createExerciseForm : Pages.CreateExerciseForm.Form
    , editExerciseForm : Maybe Pages.EditExerciseForm.Form
    , showDayPage : Maybe Pages.ShowDayPage.Page
    , store : Store
    , seed : Random.Seed
    , today : Date
    }


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Loading
        { seed = LoadingValue
        , store = LoadingValue
        , today = LoadingValue
        , url = url
        , key = key
        }
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
    = RouterMsg Router.RouteMsg
    | ReceiveStore Json.Encode.Value
    | CreateSeed Time.Posix
    | ReceiveToday Date
    | CreateExerciseMsg Pages.CreateExerciseForm.Msg
    | EditExerciseMsg Pages.EditExerciseForm.Msg
    | ShowDayMsg Pages.ShowDayPage.Msg


goToMainPageCmd : LoadedModel -> Cmd Msg
goToMainPageCmd model =
    Router.changeRoute model.router Router.ListNextDays


updateLoading : Msg -> LoadingModel -> ( Model, Cmd Msg )
updateLoading msg model =
    case msg of
        RouterMsg _ ->
            Debug.log "RouterMsg should not be called in the loading model" ( Loading model, Cmd.none )

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
                            , Cmd.none
                              --Nav.pushUrl updatedModel.router.key (Url.toString updatedModel.router.url)
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
                    , Cmd.none
                      --Nav.pushUrl updatedModel.router.key (Url.toString updatedModel.router.url)
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
                    , Cmd.none
                      --Nav.pushUrl updatedModel.router.key (Url.toString updatedModel.router.url)
                    )

                Nothing ->
                    ( Loading updatedModel, Cmd.none )

        CreateExerciseMsg _ ->
            Debug.log "CreateExerciseMsg should not be called in the loading model" ( Loading model, Cmd.none )

        EditExerciseMsg _ ->
            Debug.log "EditExerciseMsg should not be called in the loading model" ( Loading model, Cmd.none )

        ShowDayMsg _ ->
            Debug.log "ShowDayMsg should not be called in the loading model" ( Loading model, Cmd.none )


updateLoaded : Msg -> LoadedModel -> ( Model, Cmd Msg )
updateLoaded msg model =
    case msg of
        RouterMsg routerMsg ->
            let
                ( newRouter, cmd ) =
                    Router.update routerMsg model.router
            in
            case Router.getRoute newRouter of
                Router.CreateExercise ->
                    ( Loaded
                        { model
                            | router = newRouter
                            , createExerciseForm = Pages.CreateExerciseForm.init
                        }
                    , cmd
                    )

                Router.DeleteExercise id ->
                    let
                        existingExercises =
                            Storage.getExercises model.store

                        deletedExercises =
                            List.filter (\exercise -> exercise.id == id) existingExercises

                        keepedExercises =
                            List.filter (\exercise -> exercise.id /= id) existingExercises

                        maybeDeletedExerciseDate =
                            case deletedExercises of
                                deletedExercise :: _ ->
                                    let
                                        exercisesInDate =
                                            List.filter (\exercise -> exercise.date == deletedExercise.date) keepedExercises
                                    in
                                    case List.length exercisesInDate of
                                        0 ->
                                            Nothing

                                        _ ->
                                            Just deletedExercise.date

                                _ ->
                                    Nothing

                        newStore =
                            Storage.setExercises keepedExercises model.store

                        -- ( updatedLongPress, longPressCmd ) =
                        --     Gestures.updateLongPress
                        --         Gestures.Reset
                        --         model.longPress
                        --         (\e -> ExerciseLongPress e) TODO
                    in
                    ( Loaded
                        { model
                            | router = newRouter
                            , store = newStore

                            -- , longPress = updatedLongPress TODO
                        }
                    , Cmd.batch
                        [ Storage.save newStore
                        , case maybeDeletedExerciseDate of
                            Just date ->
                                Router.changeRoute model.router (Router.ShowDay date)

                            Nothing ->
                                goToMainPageCmd model
                        , cmd

                        -- , longPressCmd TODO
                        ]
                    )

                EditExercise id ->
                    let
                        maybeExercise =
                            List.filter (\e -> e.id == id) (Storage.getExercises model.store)
                                |> List.head
                    in
                    case maybeExercise of
                        Just exercise ->
                            ( Loaded
                                { model
                                    | editExerciseForm = Just (Pages.EditExerciseForm.init exercise)
                                    , router = newRouter
                                }
                            , Cmd.none
                            )

                        Nothing ->
                            ( Loaded { model | router = newRouter }, cmd )

                ShowDay date ->
                    ( Loaded
                        { model
                            | showDayPage = Just (Pages.ShowDayPage.init date)
                            , router = newRouter
                        }
                    , cmd
                    )

                _ ->
                    ( Loaded { model | router = newRouter }, cmd )

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
            case Router.getRoute model.router of
                CreateExercise ->
                    case createFormMsg of
                        Pages.CreateExerciseForm.FormMsg formMsg ->
                            let
                                newForm =
                                    Pages.CreateExerciseForm.update formMsg model.createExerciseForm
                            in
                            case ( formMsg, Pages.CreateExerciseForm.getOutput newForm model.seed ) of
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
                                        , goToMainPageCmd model
                                        ]
                                    )

                                _ ->
                                    ( Loaded { model | createExerciseForm = newForm }, Cmd.none )

                        Pages.CreateExerciseForm.Cancel ->
                            ( Loaded model, goToMainPageCmd model )

                _ ->
                    ( Loaded model, Cmd.none )

        EditExerciseMsg editFormMsg ->
            case Router.getRoute model.router of
                EditExercise id ->
                    let
                        maybeExercise =
                            List.filter (\e -> e.id == id) (Storage.getExercises model.store)
                                |> List.head
                    in
                    case maybeExercise of
                        Just exercise ->
                            case editFormMsg of
                                Pages.EditExerciseForm.FormMsg formMsg ->
                                    let
                                        newForm =
                                            case model.editExerciseForm of
                                                Just form ->
                                                    Pages.EditExerciseForm.update formMsg form

                                                Nothing ->
                                                    Pages.EditExerciseForm.init exercise
                                    in
                                    case ( formMsg, Pages.EditExerciseForm.getOutput newForm exercise ) of
                                        ( Form.Submit, Just updatedExercise ) ->
                                            let
                                                newStore =
                                                    Storage.setExercises
                                                        (List.map
                                                            (\e ->
                                                                if e.id == id then
                                                                    updatedExercise

                                                                else
                                                                    e
                                                            )
                                                            (Storage.getExercises model.store)
                                                        )
                                                        model.store
                                            in
                                            ( Loaded
                                                { model | store = newStore }
                                            , Cmd.batch
                                                [ Storage.save newStore
                                                , Router.changeRoute model.router (Router.ShowDay updatedExercise.date)
                                                ]
                                            )

                                        _ ->
                                            ( Loaded { model | editExerciseForm = Just newForm }, Cmd.none )

                                Pages.EditExerciseForm.Cancel ->
                                    ( Loaded model, Router.changeRoute model.router (Router.ShowDay exercise.date) )

                        Nothing ->
                            Debug.log "There is no exercises with this ID" ( Loaded model, goToMainPageCmd model )

                _ ->
                    Debug.log "This should not happen" ( Loaded model, Cmd.none )

        ShowDayMsg showDayMsg ->
            case model.showDayPage of
                Just showDayPage ->
                    let
                        ( newPage, cmd ) =
                            Pages.ShowDayPage.update showDayMsg showDayPage
                    in
                    case showDayMsg of
                        ToggleValidated id ->
                            let
                                newStore =
                                    Storage.toggleValidated model.store id
                            in
                            ( Loaded { model | showDayPage = Just newPage, store = newStore }
                            , Cmd.batch
                                [ Cmd.map (\x -> ShowDayMsg x) cmd
                                , Storage.save newStore
                                ]
                            )

                        _ ->
                            ( Loaded { model | showDayPage = Just newPage }
                            , Cmd.map (\x -> ShowDayMsg x) cmd
                            )

                Nothing ->
                    Debug.log "This should not happen" ( Loaded model, Cmd.none )


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
            let
                router =
                    Router.initRouter loading.url loading.key
            in
            Just
                { router = router
                , nextDaysPage = Just (Pages.NextDaysPage.init today)
                , pastDaysPage = Just (Pages.PastDaysPage.init today)
                , createExerciseForm = Pages.CreateExerciseForm.init
                , editExerciseForm =
                    case Router.getRoute router of
                        EditExercise id ->
                            let
                                maybeExercise =
                                    case List.filter (\x -> x.id == id) (Storage.getExercises store) of
                                        exercise :: _ ->
                                            Just exercise

                                        _ ->
                                            Nothing
                            in
                            case maybeExercise of
                                Just exercise ->
                                    Just (Pages.EditExerciseForm.init exercise)

                                Nothing ->
                                    Nothing

                        _ ->
                            Nothing
                , showDayPage =
                    case Router.getRoute router of
                        ShowDay date ->
                            Just (Pages.ShowDayPage.init date)

                        _ ->
                            Nothing
                , store = store
                , seed = seed
                , today = today
                }

        _ ->
            Nothing


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Storage.receive ReceiveStore
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = strings.applicationName
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
            Html.text strings.errorSeedLoading

        ( _, LoadingError error ) ->
            Html.text (strings.errorStoreLoading ++ error)

        ( _, _ ) ->
            Html.text strings.loading


viewLoaded : LoadedModel -> Html Msg
viewLoaded model =
    case Router.getRoute model.router of
        NotFound ->
            viewNotFound

        ListNextDays ->
            case model.nextDaysPage of
                Just nextDaysPage ->
                    Pages.NextDaysPage.view model.store nextDaysPage

                Nothing ->
                    viewNotFound

        ListPastDays ->
            case model.pastDaysPage of
                Just pastDaysPage ->
                    Pages.PastDaysPage.view model.store pastDaysPage

                Nothing ->
                    viewNotFound

        CreateExercise ->
            viewCreateExercise model.createExerciseForm

        EditExercise _ ->
            case model.editExerciseForm of
                Just form ->
                    viewEditExercise form

                Nothing ->
                    viewNotFound

        DeleteExercise id ->
            Html.text (strings.deletingExercise (Exercise.idToString id))

        ShowDay date ->
            case model.showDayPage of
                Just showDayPage ->
                    Html.map (\x -> ShowDayMsg x) (Pages.ShowDayPage.view model.store showDayPage)

                Nothing ->
                    viewNotFound


viewNotFound : Html Msg
viewNotFound =
    Html.text strings.pageNotFound


viewCreateExercise : Pages.CreateExerciseForm.Form -> Html Msg
viewCreateExercise form =
    Html.map CreateExerciseMsg (Pages.CreateExerciseForm.view form)


viewEditExercise : Pages.EditExerciseForm.Form -> Html Msg
viewEditExercise form =
    Html.map EditExerciseMsg (Pages.EditExerciseForm.view form)
