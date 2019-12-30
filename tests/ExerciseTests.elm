module ExerciseTests exposing (suite)

import Date
import Exercise
import Expect
import Json.Decode
import Random
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Exercise module tests"
        [ describe "Exercise JSON encode/decoding"
            [ test "an encoded exercise can be decoded" <|
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

                        resultEncoded =
                            case resultExercise of
                                Ok exercise ->
                                    Ok (Exercise.encode exercise)

                                Err error ->
                                    Err error

                        resultDecoded =
                            case resultEncoded of
                                Ok encoded ->
                                    case Json.Decode.decodeValue Exercise.decoder encoded of
                                        Ok decoded ->
                                            Ok decoded

                                        Err error ->
                                            Err (Json.Decode.errorToString error)

                                Err error ->
                                    Err error
                    in
                    case ( resultExercise, resultDecoded ) of
                        ( Ok exercise, Ok decoded ) ->
                            Expect.equal exercise decoded

                        ( Err _, _ ) ->
                            Expect.fail "could not create the exercise"

                        ( _, Err error ) ->
                            Expect.fail error
            ]
        ]
