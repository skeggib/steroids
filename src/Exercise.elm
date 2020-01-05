module Exercise exposing (Exercise, Id, createId, decoder, encode, idFromString, idToString)

import Date exposing (Date)
import Json.Decode exposing (Decoder, andThen, fail, int, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Random
import UUID exposing (UUID)


type alias Exercise =
    { id : Id
    , name : String
    , setsNumber : Int
    , repetitionsNumber : Int
    , date : Date
    }


decoder : Decoder Exercise
decoder =
    succeed Exercise
        |> required "id" idDecoder
        |> required "name" string
        |> required "setsNumber" int
        |> required "repetitionsNumber" int
        |> required "date" dateDecoder


encode : Exercise -> Json.Encode.Value
encode exercise =
    Json.Encode.object
        [ ( "id", Json.Encode.string (idToString exercise.id) )
        , ( "name", Json.Encode.string exercise.name )
        , ( "setsNumber", Json.Encode.int exercise.setsNumber )
        , ( "repetitionsNumber", Json.Encode.int exercise.repetitionsNumber )
        , ( "date", Json.Encode.string (Date.toIsoString exercise.date) )
        ]


idDecoder : Decoder Id
idDecoder =
    string
        |> andThen
            (\idString ->
                case idFromString idString of
                    Just id ->
                        succeed id

                    Nothing ->
                        fail "Could not parse ID"
            )


dateDecoder : Decoder Date
dateDecoder =
    string
        |> andThen
            (\dateString ->
                case Date.fromIsoString dateString of
                    Ok date ->
                        succeed date

                    Err error ->
                        fail error
            )


type Id
    = Id UUID


createId : Random.Seed -> ( Id, Random.Seed )
createId seed =
    let
        ( uuid, newSeed ) =
            Random.step UUID.generator seed
    in
    ( Id uuid, newSeed )


idToString : Id -> String
idToString id =
    case id of
        Id uuid ->
            UUID.toString uuid


idFromString : String -> Maybe Id
idFromString string =
    case UUID.fromString string of
        Ok uuid ->
            Just (Id uuid)

        Err _ ->
            Nothing
