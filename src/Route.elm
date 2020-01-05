module Route exposing (Route(..), parseRoute)

import CreateExerciseForm
import Date exposing (Date)
import Exercise
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, custom, map, oneOf, parse, s, top)


type Route
    = ListExercises
    | CreateExercise CreateExerciseForm.Form
    | DeleteExercise Exercise.Id
    | ShowDay Date
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map ListExercises top
        , map (CreateExercise CreateExerciseForm.init) (s "exercises" </> s "create")
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


parseRoute : Url -> Route
parseRoute url =
    case parse routeParser url of
        Just route ->
            route

        Nothing ->
            NotFound
