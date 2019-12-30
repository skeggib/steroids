module Exercise exposing (Exercise, Id, createId, idToString, idFromString)

import Date
import Random
import UUID exposing (UUID)


type alias Exercise =
    { id : Id
    , name : String
    , setsNumber : Int
    , repetitionsNumber : Int
    , date : Date.Date
    }


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
        Ok uuid -> Just (Id uuid)
        Err _ -> Nothing
