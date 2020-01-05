module Route exposing (Route(..), parseRoute, toLink)

import Date exposing (Date)
import Exercise
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, custom, map, oneOf, parse, s, top)


type Route
    = ListNextDays
    | ListPastDays
    | CreateExercise
    | DeleteExercise Exercise.Id
    | ShowDay Date
    | NotFound


toLink : Route -> String
toLink route =
    case route of
        ListNextDays ->
            "/"

        ListPastDays ->
            "/past"

        CreateExercise ->
            "/exercises/create"

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
