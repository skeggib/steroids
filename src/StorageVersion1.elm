module StorageVersion1 exposing (Store(..), decoder, encode, getExercises, init, setExercises)

import ExerciseVersion1 as Exercise
import Json.Decode exposing (Decoder, list, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode


type Store
    = Store Model


type alias Model =
    { exercises : List Exercise.Exercise
    }


init : Store
init =
    Store
        { exercises = []
        }


getExercises : Store -> List Exercise.Exercise
getExercises store =
    case store of
        Store record ->
            record.exercises


setExercises : List Exercise.Exercise -> Store -> Store
setExercises exercises store =
    case store of
        Store record ->
            Store { record | exercises = exercises }



-- Json encoding/decoding


encode : Store -> Json.Encode.Value
encode store =
    case store of
        Store { exercises } ->
            Json.Encode.object
                [ ( "version", Json.Encode.int 1 )
                , ( "exercises", Json.Encode.list Exercise.encode exercises )
                ]


decoderExercisesList : Decoder (List Exercise.Exercise)
decoderExercisesList =
    list Exercise.decoder


decoderModel : Decoder Model
decoderModel =
    succeed Model
        |> required "exercises" decoderExercisesList


decoder : Decoder Store
decoder =
    Json.Decode.map (\model -> Store model) decoderModel
