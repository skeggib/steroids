module Main exposing (main)

import Bootstrap exposing (ButtonStyle(..))
import Browser
import Browser.Navigation as Nav
import Date exposing (Date)
import Form
import Html exposing (Html)
import Json.Decode
import Json.Encode
import Model.ExerciseVersion2 as Exercise
import Model.StorageVersion2 as Storage exposing (Store)
import Pages.CreateExerciseForm
import Pages.EditExerciseForm
import Pages.NextDaysPage
import Pages.PastDaysPage
import Pages.ShowDayPage exposing (Msg(..))
import Random
import Router exposing (Route(..))
import Task
import Time
import Url
import Words exposing (strings)


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
    , page : Page
    , store : Store
    , seed : Random.Seed
    , today : Date
    }


type Page
    = NextDaysPage Pages.NextDaysPage.Page
    | PastDaysPage Pages.PastDaysPage.Page
    | CreateExercisePage Pages.CreateExerciseForm.Form
    | EditExercisePage Pages.EditExerciseForm.Form
    | ShowDayPage Pages.ShowDayPage.Page
    | NotFoundPage


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


goToMainPageCmd : Router.Router -> Cmd Msg
goToMainPageCmd router =
    Router.changeRoute router Router.ListNextDays


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
                ( newRouter, routerCmd ) =
                    Router.update routerMsg model.router

                newPage =
                    pageFromRoute (Router.getRoute newRouter) model.today model.store

                cmd =
                    routerCmd
            in
            case Router.getRoute newRouter of
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
                    in
                    ( Loaded
                        { model
                            | router = newRouter
                            , store = newStore
                            , page = newPage
                        }
                    , Cmd.batch
                        [ Storage.save newStore
                        , case maybeDeletedExerciseDate of
                            Just date ->
                                Router.changeRoute model.router (Router.ShowDay date)

                            Nothing ->
                                goToMainPageCmd model.router
                        , cmd
                        ]
                    )

                _ ->
                    ( Loaded
                        { model
                            | router = newRouter
                            , page = newPage
                        }
                    , cmd
                    )

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
            case model.page of
                CreateExercisePage page ->
                    case createFormMsg of
                        Pages.CreateExerciseForm.FormMsg formMsg ->
                            let
                                newForm =
                                    Pages.CreateExerciseForm.update formMsg page
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
                                        , goToMainPageCmd model.router
                                        ]
                                    )

                                _ ->
                                    ( Loaded { model | page = CreateExercisePage newForm }, Cmd.none )

                        Pages.CreateExerciseForm.Cancel ->
                            ( Loaded model, goToMainPageCmd model.router )

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
                                            case model.page of
                                                EditExercisePage page ->
                                                    Pages.EditExerciseForm.update formMsg page

                                                _ ->
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
                                            ( Loaded { model | page = EditExercisePage newForm }, Cmd.none )

                                Pages.EditExerciseForm.Cancel ->
                                    ( Loaded model, Router.changeRoute model.router (Router.ShowDay exercise.date) )

                        Nothing ->
                            Debug.log "There is no exercises with this ID" ( Loaded model, goToMainPageCmd model.router )

                _ ->
                    Debug.log "This should not happen" ( Loaded model, Cmd.none )

        ShowDayMsg showDayMsg ->
            case model.page of
                ShowDayPage page ->
                    let
                        ( newPage, cmd ) =
                            Pages.ShowDayPage.update showDayMsg page
                    in
                    case showDayMsg of
                        ToggleValidated id ->
                            let
                                newStore =
                                    Storage.toggleValidated model.store id
                            in
                            ( Loaded { model | page = ShowDayPage newPage, store = newStore }
                            , Cmd.batch
                                [ Cmd.map (\x -> ShowDayMsg x) cmd
                                , Storage.save newStore
                                ]
                            )

                        _ ->
                            ( Loaded { model | page = ShowDayPage newPage }
                            , Cmd.map (\x -> ShowDayMsg x) cmd
                            )

                _ ->
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

                page =
                    pageFromRoute (Router.getRoute router) today store
            in
            Just
                { router = router
                , page = page
                , store = store
                , seed = seed
                , today = today
                }

        _ ->
            Nothing


pageFromRoute : Router.Route -> Date -> Store -> Page
pageFromRoute route today store =
    case route of
        ListNextDays ->
            NextDaysPage (Pages.NextDaysPage.init today)

        ListPastDays ->
            PastDaysPage (Pages.PastDaysPage.init today)

        CreateExercise ->
            CreateExercisePage Pages.CreateExerciseForm.init

        EditExercise exerciseId ->
            let
                maybeExercise =
                    case List.filter (\exercise -> exercise.id == exerciseId) (Storage.getExercises store) of
                        exercise :: _ ->
                            Just exercise

                        _ ->
                            Nothing
            in
            case maybeExercise of
                Just exercise ->
                    EditExercisePage (Pages.EditExerciseForm.init exercise)

                Nothing ->
                    NotFoundPage

        DeleteExercise _ ->
            Debug.log "This should not happen" NotFoundPage

        ShowDay date ->
            ShowDayPage (Pages.ShowDayPage.init date)

        NotFound ->
            NotFoundPage


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
    case model.page of
        NotFoundPage ->
            viewNotFound

        NextDaysPage page ->
            Pages.NextDaysPage.view model.store page

        PastDaysPage page ->
            Pages.PastDaysPage.view model.store page

        CreateExercisePage page ->
            viewCreateExercise page

        EditExercisePage page ->
            viewEditExercise page

        ShowDayPage page ->
            Html.map (\x -> ShowDayMsg x) (Pages.ShowDayPage.view model.store page)


viewNotFound : Html Msg
viewNotFound =
    Html.text strings.pageNotFound


viewCreateExercise : Pages.CreateExerciseForm.Form -> Html Msg
viewCreateExercise form =
    Html.map CreateExerciseMsg (Pages.CreateExerciseForm.view form)


viewEditExercise : Pages.EditExerciseForm.Form -> Html Msg
viewEditExercise form =
    Html.map EditExerciseMsg (Pages.EditExerciseForm.view form)
