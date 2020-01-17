module StorageVersionTests exposing (suite)

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
    let
        ( id1v1, seed1v1 ) =
            ExerciseVersion1.createId (Random.initialSeed 0)

        ( id2v1, _ ) =
            ExerciseVersion1.createId seed1v1

        exercise1v1 =
            { id = id1v1
            , name = "test 1"
            , setsNumber = 10
            , repetitionsNumber = 5
            , date = Date.fromCalendarDate 2020 Time.Jan 19
            }

        exercise2v1 =
            { id = id2v1
            , name = "test 2"
            , setsNumber = 5
            , repetitionsNumber = 10
            , date = Date.fromCalendarDate 2020 Time.Jan 18
            }

        expectedv1 =
            StorageVersion1.setExercises [ exercise1v1, exercise2v1 ] StorageVersion1.init

        jsonNoVersion =
            """
            {
                "exercises": [
                    {
                        "id": """ ++ "\"" ++ ExerciseVersion1.idToString exercise1v1.id ++ "\"" ++ """,
                        "name": "test 1",
                        "setsNumber": 10,
                        "repetitionsNumber": 5,
                        "date": "2020-01-19"
                    },
                    {
                        "id": """ ++ "\"" ++ ExerciseVersion1.idToString exercise2v1.id ++ "\"" ++ """,
                        "name": "test 2",
                        "setsNumber": 5,
                        "repetitionsNumber": 10,
                        "date": "2020-01-18"
                    }
                ]
            }
            """

        jsonv1 =
            """
            {
                "version": 1,
                "exercises": [
                    {
                        "id": """ ++ "\"" ++ ExerciseVersion1.idToString exercise1v1.id ++ "\"" ++ """,
                        "name": "test 1",
                        "setsNumber": 10,
                        "repetitionsNumber": 5,
                        "date": "2020-01-19"
                    },
                    {
                        "id": """ ++ "\"" ++ ExerciseVersion1.idToString exercise2v1.id ++ "\"" ++ """,
                        "name": "test 2",
                        "setsNumber": 5,
                        "repetitionsNumber": 10,
                        "date": "2020-01-18"
                    }
                ]
            }
            """

        ( id1v2, seed1v2 ) =
            ExerciseVersion2.createId (Random.initialSeed 0)

        ( id2v2, _ ) =
            ExerciseVersion2.createId seed1v2

        exercise1v2 =
            { id = id1v2
            , name = "test 1"
            , setsNumber = 10
            , repetitionsNumber = 5
            , date = Date.fromCalendarDate 2020 Time.Jan 19
            , validated = False
            }

        exercise2v2 =
            { id = id2v2
            , name = "test 2"
            , setsNumber = 5
            , repetitionsNumber = 10
            , date = Date.fromCalendarDate 2020 Time.Jan 18
            , validated = False
            }

        expectedv2 =
            StorageVersion2.setExercises [ exercise1v2, exercise2v2 ] StorageVersion2.init

        jsonv2 =
            """
            {
                "version": 2,
                "exercises": [
                    {
                        "id": """ ++ "\"" ++ ExerciseVersion2.idToString exercise1v2.id ++ "\"" ++ """,
                        "name": "test 1",
                        "setsNumber": 10,
                        "repetitionsNumber": 5,
                        "date": "2020-01-19",
                        "validated": false
                    },
                    {
                        "id": """ ++ "\"" ++ ExerciseVersion2.idToString exercise2v2.id ++ "\"" ++ """,
                        "name": "test 2",
                        "setsNumber": 5,
                        "repetitionsNumber": 10,
                        "date": "2020-01-18",
                        "validated": false
                    }
                ]
            }
            """
    in
    describe "Storage version tests"
        [ describe "Parsing tests"
            [ test "a version 1 JSON can be decoded" <|
                \_ ->
                    Expect.equal
                        (Ok expectedv1)
                        (Json.Decode.decodeString StorageVersion1.decoder jsonv1)
            , test "a version 2 JSON can be decoded" <|
                \_ ->
                    Expect.equal
                        (Ok expectedv2)
                        (Json.Decode.decodeString StorageVersion2.decoder jsonv2)
            ]
        , describe "Conversion tests"
            [ test "a no version JSON can be converted into the latest version store" <|
                \_ ->
                    Expect.equal
                        (Ok expectedv2)
                        (Json.Decode.decodeString StorageVersion2.decoder jsonNoVersion)
            , test "a version 1 JSON can be converted into the latest version store" <|
                \_ ->
                    Expect.equal
                        (Ok expectedv2)
                        (Json.Decode.decodeString StorageVersion2.decoder jsonv1)
            ]
        ]
