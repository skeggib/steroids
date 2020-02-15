module Router exposing (Route(..), RouteMsg, Router, changeRoute, getRoute, initRouter, onUrlChanged, onUrlRequested, parseRoute, toLink, update)

import Browser
import Browser.Navigation as Nav
import Date exposing (Date)
import ExerciseVersion2 as Exercise
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, custom, map, oneOf, parse, s, top)


type RouteMsg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


onUrlRequested : Browser.UrlRequest -> RouteMsg
onUrlRequested =
    UrlRequested


onUrlChanged : Url.Url -> RouteMsg
onUrlChanged =
    UrlChanged


type Route
    = ListNextDays
    | ListPastDays
    | CreateExercise
    | EditExercise Exercise.Id
    | DeleteExercise Exercise.Id
    | ShowDay Date
    | NotFound


type Router
    = Router
        { url : Url.Url
        , key : Nav.Key
        , route : Route
        }


initRouter : Url.Url -> Nav.Key -> Router
initRouter url key =
    Router
        { url = url
        , key = key
        , route = parseRoute url
        }


changeRoute : Router -> Route -> Cmd msg
changeRoute router route =
    Nav.pushUrl (getKey router) (toLink route)


getKey : Router -> Nav.Key
getKey router =
    case router of
        Router routerValues ->
            routerValues.key


setUrl : Url.Url -> Router -> Router
setUrl url router =
    case router of
        Router routerValues ->
            Router { routerValues | url = url }


setRoute : Route -> Router -> Router
setRoute route router =
    case router of
        Router routerValues ->
            Router { routerValues | route = route }


getRoute : Router -> Route
getRoute router =
    case router of
        Router routerValues ->
            routerValues.route


update : RouteMsg -> Router -> ( Router, Cmd msg )
update msg router =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( router, Nav.pushUrl (getKey router) (Url.toString url) )

                Browser.External href ->
                    ( router, Nav.load href )

        UrlChanged url ->
            let
                newRoute =
                    parseRoute url

                newRouter =
                    router
                        |> setUrl url
                        |> setRoute newRoute
            in
            ( newRouter, Cmd.none )


toLink : Route -> String
toLink route =
    case route of
        ListNextDays ->
            "/"

        ListPastDays ->
            "/past"

        CreateExercise ->
            "/exercises/create"

        EditExercise id ->
            "/exercises/edit/" ++ Exercise.idToString id

        DeleteExercise id ->
            "/exercises/delete/" ++ Exercise.idToString id

        ShowDay date ->
            "/day/" ++ dateToString date

        NotFound ->
            "/notfound"


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map ListNextDays top
        , map ListPastDays (s "past")
        , map CreateExercise (s "exercises" </> s "create")
        , map EditExercise (s "exercises" </> s "edit" </> custom "exerciseId" Exercise.idFromString)
        , map DeleteExercise (s "exercises" </> s "delete" </> custom "exerciseId" Exercise.idFromString)
        , map ShowDay (s "day" </> custom "date" dateFromString)
        ]


dateFromString : String -> Maybe Date
dateFromString str =
    case Date.fromIsoString str of
        Ok date ->
            Just date

        Err _ ->
            Nothing


dateToString : Date -> String
dateToString date =
    Date.toIsoString date


parseRoute : Url -> Route
parseRoute url =
    case parse routeParser url of
        Just route ->
            route

        Nothing ->
            NotFound
