port module Storage exposing (Store, decoder, encode, getExercises, init, receive, request, save, setExercises)

import Exercise exposing (Exercise)
import Json.Decode exposing (Decoder, list, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode


type Store
    = Store Model


type alias Model =
    { exercises : List Exercise
    }


port request : () -> Cmd msg


port receive : (Json.Encode.Value -> msg) -> Sub msg


save : Store -> Cmd msg
save store =
    savePort (encode store)


port savePort : Json.Encode.Value -> Cmd msg


init : Store
init =
    Store
        { exercises = []
        }


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



-- Json encoding/decoding


encode : Store -> Json.Encode.Value
encode store =
    case store of
        Store { exercises } ->
            Json.Encode.object
                [ ( "exercises", Json.Encode.list Exercise.encode exercises )
                ]


decoderExercisesList : Decoder (List Exercise)
decoderExercisesList =
    list Exercise.decoder


decoderModel : Decoder Model
decoderModel =
    succeed Model
        |> required "exercises" decoderExercisesList


decoder : Decoder Store
decoder =
    Json.Decode.map (\model -> Store model) decoderModel
