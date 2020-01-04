module Route exposing (Route(..), parseRoute)

import CreateExerciseForm
import Exercise
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, custom, map, oneOf, parse, s, top)


type Route
    = ListExercises
    | CreateExercise CreateExerciseForm.Form
    | DeleteExercise Exercise.Id
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map ListExercises top
        , map ListExercises (s "exercises")
        , map (CreateExercise CreateExerciseForm.init) (s "exercises" </> s "create")
        , map DeleteExercise (s "exercises" </> s "delete" </> custom "exerciseId" Exercise.idFromString)
        ]


parseRoute : Url -> Route
parseRoute url =
    case parse routeParser url of
        Just route ->
            route

        Nothing ->
            NotFound
