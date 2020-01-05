module StorageTests exposing (suite)

import Date
import Exercise
import Expect
import Json.Decode
import Random
import Storage
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "Storage module tests"
        [ describe "Store JSON encode/decoding"
            [ test "an encoded store can be decoded" <|
                \_ ->
                    let
                        date =
                            Date.fromCalendarDate 30 Time.Mar 2012

                        id =
                            Exercise.createId (Random.initialSeed 0)

                        exercise =
                            { id = Tuple.first id
                            , name = "test"
                            , setsNumber = 5
                            , repetitionsNumber = 10
                            , date = date
                            }

                        store =
                            Storage.setExercises [ exercise ] Storage.init

                        encoded =
                            Storage.encode store

                        resultDecoded =
                            case Json.Decode.decodeValue Storage.decoder encoded of
                                Ok decoded ->
                                    Ok decoded

                                Err error ->
                                    Err (Json.Decode.errorToString error)
                    in
                    case resultDecoded of
                        Ok decoded ->
                            Expect.equal store decoded

                        Err error ->
                            Expect.fail error
            ]
        ]
