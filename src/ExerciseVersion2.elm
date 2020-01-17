module ExerciseVersion2 exposing (Exercise, Id, createId, decoder, encode, idFromString, idToString, v1Tov2Converter)

import Date exposing (Date)
import ExerciseVersion1
import Json.Decode exposing (Decoder, bool, int, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Random


type alias Exercise =
    { id : Id
    , name : String
    , setsNumber : Int
    , repetitionsNumber : Int
    , date : Date
    , validated : Bool
    }


decoder : Decoder Exercise
decoder =
    succeed Exercise
        |> required "id" idDecoder
        |> required "name" string
        |> required "setsNumber" int
        |> required "repetitionsNumber" int
        |> required "date" dateDecoder
        |> required "validated" bool


encode : Exercise -> Json.Encode.Value
encode exercise =
    Json.Encode.object
        [ ( "id", Json.Encode.string (idToString exercise.id) )
        , ( "name", Json.Encode.string exercise.name )
        , ( "setsNumber", Json.Encode.int exercise.setsNumber )
        , ( "repetitionsNumber", Json.Encode.int exercise.repetitionsNumber )
        , ( "date", Json.Encode.string (Date.toIsoString exercise.date) )
        , ( "validated", Json.Encode.bool exercise.validated )
        ]


v1Tov2Converter : ExerciseVersion1.Exercise -> Exercise
v1Tov2Converter v1Exercise =
    { id = v1Exercise.id
    , name = v1Exercise.name
    , setsNumber = v1Exercise.setsNumber
    , repetitionsNumber = v1Exercise.repetitionsNumber
    , date = v1Exercise.date
    , validated = False
    }


type alias Id =
    ExerciseVersion1.Id


createId : Random.Seed -> ( Id, Random.Seed )
createId =
    ExerciseVersion1.createId


idDecoder : Decoder Id
idDecoder =
    ExerciseVersion1.idDecoder


dateDecoder : Decoder Date
dateDecoder =
    ExerciseVersion1.dateDecoder


idToString : Id -> String
idToString =
    ExerciseVersion1.idToString


idFromString : String -> Maybe Id
idFromString =
    ExerciseVersion1.idFromString
