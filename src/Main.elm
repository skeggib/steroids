module Main exposing (main)

import Bootstrap exposing (ButtonStyle(..), buttonHyperlink, col, row)
import Browser
import Browser.Navigation as Nav
import CreateExerciseForm
import Date exposing (Date)
import Dict exposing (Dict)
import Dict.Extra
import EditExerciseForm
import ExerciseVersion2 as Exercise exposing (Exercise)
import Form
import Gestures
import Helpers
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Json.Decode
import Json.Encode
import Random
import Router exposing (Route(..), parseRoute)
import StorageVersion2 as Storage exposing (Store)
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
    { router : Router.Router
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
    , createExerciseForm : CreateExerciseForm.Form
    , editExerciseForm : Maybe EditExerciseForm.Form
    , store : Store
    , seed : Random.Seed
    , today : Date
    , longPress : Gestures.LongPressModel Exercise
    }


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Loading
        { seed = LoadingValue
        , store = LoadingValue
        , today = LoadingValue
        , router =
            { url = url
            , key = key
            , route = Router.NotFound
            }
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
    | CreateExerciseMsg CreateExerciseForm.Msg
    | EditExerciseMsg EditExerciseForm.Msg
    | ToggleValidated Exercise.Id
    | ExerciseLongPress (Gestures.LongPressEvent Exercise)


goToMainPageCmd : LoadedModel -> Cmd Msg
goToMainPageCmd model =
    Nav.pushUrl model.router.key (Router.toLink Router.ListNextDays)


updateLoading : Msg -> LoadingModel -> ( Model, Cmd Msg )
updateLoading msg model =
    case msg of
        RouterMsg routeMsg ->
            let
                routerTuple =
                    Router.update routeMsg model.router (\route -> Cmd.none)
            in
            ( Loading
                { model
                    | router = Tuple.first routerTuple
                }
            , Tuple.second routerTuple
            )

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
                            , Nav.pushUrl updatedModel.router.key (Url.toString updatedModel.router.url)
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
                    , Nav.pushUrl updatedModel.router.key (Url.toString updatedModel.router.url)
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
                    , Nav.pushUrl updatedModel.router.key (Url.toString updatedModel.router.url)
                    )

                Nothing ->
                    ( Loading updatedModel, Cmd.none )

        CreateExerciseMsg _ ->
            Debug.log "CreateExerciseMsg should not be called in the loading model" ( Loading model, Cmd.none )

        EditExerciseMsg _ ->
            Debug.log "EditExerciseMsg should not be called in the loading model" ( Loading model, Cmd.none )

        ToggleValidated _ ->
            Debug.log "ToggleValidated should not be called in the loading model" ( Loading model, Cmd.none )

        ExerciseLongPress _ ->
            Debug.log "ExercisePressEvent should not be called in the loading model" ( Loading model, Cmd.none )



-- TODO: refactor the exercise creation -> call a route that creates the exercise


routeCmd : Router.Route -> Cmd msg
routeCmd route =
    Cmd.none



-- case route of
--     DeleteExercise id ->
--         let
--             existingExercises =
--                 Storage.getExercises model.store
--             filteredExercises =
--                 List.filter (\exercise -> exercise.id /= id) existingExercises
--             newStore =
--                 Storage.setExercises filteredExercises model.store
--         in
--         ( Loaded
--             { model
--                 | url = url
--                 , route = route
--                 , store = newStore
--             }
--         , Cmd.batch
--             [ Storage.save newStore
--             , goToMainPageCmd model
--             ]
--         )
--     EditExercise id ->
--         let
--             maybeExercise =
--                 List.filter (\e -> e.id == id) (Storage.getExercises model.store)
--                     |> List.head
--         in
--         case maybeExercise of
--             Just exercise ->
--                 ( Loaded
--                     { model
--                         | editExerciseForm = Just (EditExerciseForm.init exercise)
--                         , url = url
--                         , route = route
--                     }
--                 , Cmd.none
--                 )
--             Nothing ->
--                 ( Loaded { model | url = url, route = route }, Cmd.none )
--     _ ->
--         ( Loaded { model | url = url, route = route }, Cmd.none )


updateLoaded : Msg -> LoadedModel -> ( Model, Cmd Msg )
updateLoaded msg model =
    case msg of
        RouterMsg routerMsg ->
            let
                oldRoute =
                    model.router.route

                ( newRouter, cmd ) =
                    Router.update routerMsg model.router routeCmd
            in
            case oldRoute of
                Router.CreateExercise ->
                    ( Loaded
                        { model
                            | router = newRouter
                            , createExerciseForm = CreateExerciseForm.init
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
            case model.router.route of
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
                                        , goToMainPageCmd model
                                        ]
                                    )

                                _ ->
                                    ( Loaded { model | createExerciseForm = newForm }, Cmd.none )

                        CreateExerciseForm.Cancel ->
                            ( Loaded model, goToMainPageCmd model )

                _ ->
                    ( Loaded model, Cmd.none )

        EditExerciseMsg editFormMsg ->
            case model.router.route of
                EditExercise id ->
                    let
                        maybeExercise =
                            List.filter (\e -> e.id == id) (Storage.getExercises model.store)
                                |> List.head
                    in
                    case maybeExercise of
                        Just exercise ->
                            case editFormMsg of
                                EditExerciseForm.FormMsg formMsg ->
                                    let
                                        newForm =
                                            case model.editExerciseForm of
                                                Just form ->
                                                    EditExerciseForm.update formMsg form

                                                Nothing ->
                                                    EditExerciseForm.init exercise
                                    in
                                    case ( formMsg, EditExerciseForm.getOutput newForm exercise ) of
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
                                                , goToMainPageCmd model
                                                ]
                                            )

                                        _ ->
                                            ( Loaded { model | editExerciseForm = Just newForm }, Cmd.none )

                                EditExerciseForm.Cancel ->
                                    ( Loaded model, goToMainPageCmd model )

                        Nothing ->
                            ( Loaded model, goToMainPageCmd model )

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

        ExerciseLongPress event ->
            let
                tuple =
                    Gestures.updateLongPress
                        event
                        model.longPress
                        (\e -> ExerciseLongPress e)
            in
            ( Loaded { model | longPress = Tuple.first tuple }, Tuple.second tuple )


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
                { router =
                    { key = loading.router.key
                    , url = loading.router.url
                    , route = parseRoute loading.router.url
                    }
                , createExerciseForm = CreateExerciseForm.init
                , editExerciseForm = Nothing
                , store = store
                , seed = seed
                , today = today
                , longPress = Gestures.initLongPress
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
    case model.router.route of
        NotFound ->
            viewNotFound

        ListNextDays ->
            viewNextDays model.today (Storage.getExercises model.store)

        ListPastDays ->
            viewPastDays model.today (Storage.getExercises model.store)

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
            viewDay date (Storage.getExercises model.store) model.longPress.pressing model.longPress.pressed


viewNotFound : Html Msg
viewNotFound =
    Html.text strings.pageNotFound


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
            [ buttonHyperlink
                Primary
                [ Html.Attributes.class "float-right ml-2" ]
                (Router.toLink Router.CreateExercise)
                strings.actionCreateExercise
            , buttonHyperlink
                Light
                [ Html.Attributes.class "float-right" ]
                (Router.toLink Router.ListPastDays)
                strings.actionViewPastExercises
            ]
    in
    viewDaysList days strings.titleNextDaysPage buttons


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
            [ buttonHyperlink
                Light
                [ Html.Attributes.class "float-right" ]
                (Router.toLink Router.ListNextDays)
                strings.actionGoBackToNextDays
            ]
    in
    viewDaysList days strings.titlePastDaysPage buttons


viewDaysList : List ( Date, List Exercise ) -> String -> List (Html Msg) -> Html Msg
viewDaysList days header buttons =
    viewPage
        header
        Nothing
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
                    [ Html.div [ Html.Attributes.class "d-flex justify-content-center mt-3" ] [ Html.text strings.noExercises ] ]

                 else
                    List.map viewDayLink days
                )
            )
        )


viewPage : String -> Maybe (List (Html Msg)) -> Html Msg -> Html Msg
viewPage header actionBarContent content =
    let
        actionBar =
            case actionBarContent of
                Just justActionBarContent ->
                    Html.div
                        [ Html.Attributes.class "fixed-top p-3 action-bar" ]
                        justActionBarContent

                Nothing ->
                    Html.div
                        [ Html.Attributes.class "fixed-top p-3 action-bar hidden" ]
                        []
    in
    Html.div [ Html.Attributes.style "min-height" "100vh" ]
        [ Html.div
            [ Html.Attributes.class "container"
            , Html.Attributes.style "min-height" "100vh"
            , Mouse.onDown (\_ -> ExerciseLongPress Gestures.Reset)
            ]
            [ row [] [ Html.h1 [ Html.Attributes.class "my-4" ] [ Html.text header ] |> col [] ]
            , content
            ]
        , actionBar
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
            [ Html.Attributes.href (Router.ShowDay date |> Router.toLink), Html.Attributes.class "dayLink" ]
            [ Html.span []
                [ Html.h3
                    [ Html.Attributes.class "d-inline mr-3" ]
                    [ Html.text dateStr ]
                , Html.br [] []
                , Html.span
                    [ Html.Attributes.class "text-muted" ]
                    [ Html.text (strings.numberOfExercisesInDay doneNumber exercisesLength)
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


viewExercise : Exercise -> Bool -> Bool -> Html Msg
viewExercise exercise pressing pressed =
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
    in
    Html.div
        [ Html.Attributes.class "p-2"
        , Html.Attributes.style "background-color"
            (case ( pressing, pressed ) of
                ( True, False ) ->
                    "#f5f5f5"

                ( False, True ) ->
                    "#b3e5fc"

                ( True, True ) ->
                    "#81d4fa"

                ( False, False ) ->
                    "rgba(0,0,0,0)"
            )
        ]
        [ row []
            [ col
                [ Html.Events.onClick (ToggleValidated exercise.id)
                , Html.Attributes.class "col-2"
                , Html.Attributes.class "d-flex justify-content-center align-items-center"
                ]
                checkBox
            , col
                (List.append
                    [ Html.Attributes.class "col-8 pl-0" ]
                    (Gestures.longPress
                        exercise
                        (\e -> ExerciseLongPress e)
                    )
                )
                (Html.div []
                    [ row [] [ col [] title ]
                    , row [] [ col [] text ]
                    ]
                )
            ]
        ]


viewCreateExercise : CreateExerciseForm.Form -> Html Msg
viewCreateExercise form =
    Html.map CreateExerciseMsg (CreateExerciseForm.view form)


viewEditExercise : EditExerciseForm.Form -> Html Msg
viewEditExercise form =
    Html.map EditExerciseMsg (EditExerciseForm.view form)


viewDay : Date -> List Exercise -> Maybe Exercise -> Maybe Exercise -> Html Msg
viewDay date exercises pressingExercise pressedExercise =
    let
        filteredExercises =
            List.filter (\exercise -> exercise.date == date) exercises

        actionBarContent =
            Maybe.map
                (\justPressedExercise ->
                    [ Html.a [ Html.Attributes.href (Router.toLink (Router.DeleteExercise justPressedExercise.id)) ]
                        [ Html.i [ Html.Attributes.class "material-icons float-right px-2" ]
                            [ Html.text "delete" ]
                        ]
                    , Html.a [ Html.Attributes.href (Router.toLink (Router.EditExercise justPressedExercise.id)) ]
                        [ Html.i [ Html.Attributes.class "material-icons float-right px-2" ]
                            [ Html.text "edit" ]
                        ]
                    ]
                )
                pressedExercise
    in
    viewPage
        (Helpers.dateToLongString date)
        actionBarContent
        (Html.div
            []
            (if List.length filteredExercises == 0 then
                [ Html.text strings.noExercises ]

             else
                List.map
                    (\e ->
                        let
                            pressing =
                                pressingExercise == Just e

                            pressed =
                                pressedExercise == Just e
                        in
                        viewExercise e pressing pressed
                    )
                    filteredExercises
            )
        )
