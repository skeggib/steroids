module Words exposing (plural, words)


type alias Word =
    { singular : String
    , plural : String
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
