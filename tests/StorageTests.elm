module StorageTests exposing (suite)

import Date
import Exercise
import Expect
import Json.Decode
import Random
import Storage
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Storage module tests"
        [ describe "Store JSON encode/decoding"
            [ test "an encoded store can be decoded" <|
                \_ ->
                    let
                        resultDate =
                            Date.createDate 30 3 2012

                        id =
                            Exercise.createId (Random.initialSeed 0)

                        resultExercise =
                            case resultDate of
                                Ok date ->
                                    Ok
                                        { id = Tuple.first id
                                        , name = "test"
                                        , setsNumber = 5
                                        , repetitionsNumber = 10
                                        , date = date
                                        }

                                Err error ->
                                    Err error

                        resultStore =
                            case resultExercise of
                                Ok exercise ->
                                    Ok (Storage.setExercises [ exercise ] Storage.init)

                                Err error ->
                                    Err error

                        resultEncoded =
                            case resultStore of
                                Ok store ->
                                    Ok (Storage.encode store)

                                Err error ->
                                    Err error

                        resultDecoded =
                            case resultEncoded of
                                Ok encoded ->
                                    case Json.Decode.decodeValue Storage.decoder encoded of
                                        Ok decoded ->
                                            Ok decoded

                                        Err error ->
                                            Err (Json.Decode.errorToString error)

                                Err error ->
                                    Err error
                    in
                    case ( resultStore, resultDecoded ) of
                        ( Ok store, Ok decoded ) ->
                            Expect.equal store decoded

                        ( Err _, _ ) ->
                            Expect.fail "could not create the store"

                        ( _, Err error ) ->
                            Expect.fail error
            ]
        ]
