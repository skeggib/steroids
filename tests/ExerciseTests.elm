module ExerciseTests exposing (suite)

import Date
import Exercise
import Expect
import Json.Decode
import Random
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "Exercise module tests"
        [ describe "Exercise JSON encode/decoding"
            [ test "an encoded exercise can be decoded" <|
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

                        encoded =
                            Exercise.encode exercise

                        resultDecoded =
                            case Json.Decode.decodeValue Exercise.decoder encoded of
                                Ok decoded ->
                                    Ok decoded

                                Err error ->
                                    Err (Json.Decode.errorToString error)
                    in
                    case resultDecoded of
                        Ok decoded ->
                            Expect.equal exercise decoded

                        Err error ->
                            Expect.fail error
            ]
        ]
