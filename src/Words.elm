module Words exposing (plural, strings, words)


type alias Word =
    { singular : String
    , plural : String
    }


strings =
    { actionCreateExercise = "Create an exercise"
    , actionGoBackToNextDays = "Go back to exercises list"
    , actionViewPastExercises = "View past exercises"
    , applicationName = "Steroids"
    , deletingExercise = deletingExercise
    , errorSeedLoading = "Cannot load seed"
    , errorStoreLoading = "Cannot load store: "
    , loading = "Loading..."
    , noExercises = "No exercises!"
    , numberOfExercisesInDay = numberOfExercisesInDay
    , pageNotFound = "Page not found"
    , titleNextDaysPage = "Exercises"
    , titlePastDaysPage = "Past exercises"
    }


words =
    { exercise = Word "exercise" "exercises"
    , repetition = Word "repetition" "repetitions"
    , set = Word "set" "sets"
    }


plural : Word -> Int -> String
plural word number =
    case number of
        1 ->
            word.singular

        _ ->
            word.plural


deletingExercise : String -> String
deletingExercise id =
    "Deleting exercise " ++ id ++ "..."


numberOfExercisesInDay : Int -> Int -> String
numberOfExercisesInDay numberDone total =
    String.fromInt total
        ++ " "
        ++ plural words.exercise total
        ++ " ("
        ++ String.fromInt numberDone
        ++ "/"
        ++ String.fromInt total
        ++ ")"
