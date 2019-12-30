module Storage exposing (Store, getExercises, openStore, setExercises)

import Exercise exposing (Exercise)


type Store
    = Store
        { exercises : List Exercise
        }


openStore : Store
openStore =
    Store { exercises = [] }


getExercises : Store -> List Exercise
getExercises store =
    case store of
        Store record ->
            record.exercises


setExercises : List Exercise -> Store -> Store
setExercises exercises store =
    case store of
        Store record ->
            Store { record | exercises = exercises }
