port module Model.StorageVersion2 exposing
    ( Store
    , decoder
    , encode
    , getExercises
    , init
    , receive
    , request
    , save
    , setExercises
    , toggleValidated
    )

import Json.Decode exposing (Decoder, andThen, fail, field, int, list, oneOf, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Model.ExerciseVersion2 as Exercise
import Model.StorageVersion1


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


port request : () -> Cmd msg


port receive : (Json.Encode.Value -> msg) -> Sub msg


save : Store -> Cmd msg
save store =
    savePort (encode store)


port savePort : Json.Encode.Value -> Cmd msg


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


toggleValidated : Store -> Exercise.Id -> Store
toggleValidated store exerciseId =
    let
        exercises =
            getExercises store

        filteredExercises =
            List.filter (\e -> e.id == exerciseId) exercises

        updatedExercise =
            case filteredExercises of
                exerciseToUpdate :: _ ->
                    Just { exerciseToUpdate | validated = not exerciseToUpdate.validated }

                [] ->
                    Nothing

        updatedExercisesList =
            case updatedExercise of
                Just exercise ->
                    List.map
                        (\e ->
                            if e.id == exerciseId then
                                exercise

                            else
                                e
                        )
                        exercises

                Nothing ->
                    exercises

        updatedStore =
            setExercises updatedExercisesList store
    in
    updatedStore



-- Json encoding/decoding


encode : Store -> Json.Encode.Value
encode store =
    case store of
        Store { exercises } ->
            Json.Encode.object
                [ ( "version", Json.Encode.int 2 )
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
    oneOf
        [ field "version" int
            |> andThen decoderVersion
        , v1Tov2Decoder
        ]


decoderVersion : Int -> Decoder Store
decoderVersion version =
    case version of
        1 ->
            v1Tov2Decoder

        2 ->
            v2Decoder

        _ ->
            fail ("Version " ++ String.fromInt version ++ " is not supported")


v2Decoder : Decoder Store
v2Decoder =
    Json.Decode.map (\model -> Store model) decoderModel


v1Tov2Decoder : Decoder Store
v1Tov2Decoder =
    Json.Decode.map v1Tov2Converter Model.StorageVersion1.decoder


v1Tov2Converter : Model.StorageVersion1.Store -> Store
v1Tov2Converter v1store =
    let
        v1Exercises =
            case v1store of
                Model.StorageVersion1.Store v1model ->
                    v1model.exercises
    in
    Store
        { exercises = List.map Exercise.v1Tov2Converter v1Exercises
        }
