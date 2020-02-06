module Router exposing (Route(..), RouteMsg, Router, onUrlChanged, onUrlRequested, parseRoute, toLink, update)

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


type alias Router =
    { url : Url.Url
    , key : Nav.Key
    , route : Route
    }


update : RouteMsg -> Router -> (Route -> Cmd msg) -> ( Router, Cmd msg )
update msg router action =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( router, Nav.pushUrl router.key (Url.toString url) )

                Browser.External href ->
                    ( router, Nav.load href )

        UrlChanged url ->
            let
                route =
                    parseRoute url

                newRouter =
                    { router | url = url, route = route }
            in
            ( newRouter, action route )


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
