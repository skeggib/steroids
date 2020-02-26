module StorageTests exposing (suite)

import Date
import Expect
import Json.Decode
import Model.ExerciseVersion1
import Model.ExerciseVersion2
import Model.StorageVersion1
import Model.StorageVersion2
import Random
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
                            Model.ExerciseVersion1.createId (Random.initialSeed 0)

                        exercise =
                            { id = Tuple.first id
                            , name = "test"
                            , setsNumber = 5
                            , repetitionsNumber = 10
                            , date = date
                            }

                        store =
                            Model.StorageVersion1.setExercises [ exercise ] Model.StorageVersion1.init

                        encoded =
                            Model.StorageVersion1.encode store
                    in
                    Expect.equal
                        (Ok store)
                        (Json.Decode.decodeValue Model.StorageVersion1.decoder encoded)
            , test "a v2 encoded store can be decoded" <|
                \_ ->
                    let
                        date =
                            Date.fromCalendarDate 2012 Time.Mar 30

                        id =
                            Model.ExerciseVersion2.createId (Random.initialSeed 0)

                        exercise =
                            { id = Tuple.first id
                            , name = "test"
                            , setsNumber = 5
                            , repetitionsNumber = 10
                            , date = date
                            , validated = True
                            }

                        store =
                            Model.StorageVersion2.setExercises [ exercise ] Model.StorageVersion2.init

                        encoded =
                            Model.StorageVersion2.encode store
                    in
                    Expect.equal
                        (Ok store)
                        (Json.Decode.decodeValue Model.StorageVersion2.decoder encoded)
            ]
        ]
