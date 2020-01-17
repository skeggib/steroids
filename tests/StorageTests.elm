module StorageTests exposing (suite)

import Date
import ExerciseVersion1
import ExerciseVersion2
import Expect
import Json.Decode
import Random
import StorageVersion1
import StorageVersion2
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "Storage module tests"
        [ describe "Store JSON encode/decoding"
            [ test "a v1 encoded store can be decoded" <|
                \_ ->
                    let
                        date =
                            Date.fromCalendarDate 2012 Time.Mar 30

                        id =
                            ExerciseVersion1.createId (Random.initialSeed 0)

                        exercise =
                            { id = Tuple.first id
                            , name = "test"
                            , setsNumber = 5
                            , repetitionsNumber = 10
                            , date = date
                            }

                        store =
                            StorageVersion1.setExercises [ exercise ] StorageVersion1.init

                        encoded =
                            StorageVersion1.encode store
                    in
                    Expect.equal
                        (Ok store)
                        (Json.Decode.decodeValue StorageVersion1.decoder encoded)
            , test "a v2 encoded store can be decoded" <|
                \_ ->
                    let
                        date =
                            Date.fromCalendarDate 2012 Time.Mar 30

                        id =
                            ExerciseVersion2.createId (Random.initialSeed 0)

                        exercise =
                            { id = Tuple.first id
                            , name = "test"
                            , setsNumber = 5
                            , repetitionsNumber = 10
                            , date = date
                            , validated = True
                            }

                        store =
                            StorageVersion2.setExercises [ exercise ] StorageVersion2.init

                        encoded =
                            StorageVersion2.encode store
                    in
                    Expect.equal
                        (Ok store)
                        (Json.Decode.decodeValue StorageVersion2.decoder encoded)
            ]
        ]
