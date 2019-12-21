module DateTests exposing (suite)

import Date
import Expect
import Test exposing (describe, test, Test)


suite : Test
suite =
    describe "Date module tests"
        [ describe "Date.createDate"
            [ test "can create a date with valid values" <|
                \_ ->
                    let
                        date =
                            Date.createDate 30 3 2012
                    in
                    Expect.ok date
            , test "cannot create a date with a negative day" <|
                \_ ->
                    let
                        date =
                            Date.createDate -1 3 2012
                    in
                    Expect.err date
            , test "cannot create a date with a day greater than 31" <|
                \_ ->
                    let
                        date =
                            Date.createDate 32 3 2012
                    in
                    Expect.err date
            , test "cannot create a date with a negative month" <|
                \_ ->
                    let
                        date =
                            Date.createDate 30 -1 2012
                    in
                    Expect.err date
            , test "cannot create a date with a month greater than 12" <|
                \_ ->
                    let
                        date =
                            Date.createDate 30 13 2012
                    in
                    Expect.err date
            , test "cannot create a date with a day equal to 31 in a month of 30 days" <|
                \_ ->
                    let
                        date =
                            Date.createDate 31 4 2012
                    in
                    Expect.err date
            , test "can create a date with a day equal to 31 in a month of 31 days" <|
                \_ ->
                    let
                        date =
                            Date.createDate 31 1 2012
                    in
                    Expect.ok date
            , test "cannot create a date with a day equal 29 in february of a non-bisextile year" <|
                \_ ->
                    let
                        date =
                            Date.createDate 29 2 2011
                    in
                    Expect.err date
            , test "can create a date with a day equal 29 in february of a bisextile year" <|
                \_ ->
                    let
                        date =
                            Date.createDate 29 2 2004
                    in
                    Expect.ok date
            , test "a year divisible by 4 and by 100 and not divisible by 400 is not bisextile" <|
                \_ ->
                    let
                        date =
                            Date.createDate 29 2 2100
                    in
                    Expect.err date
            , test "a year divisible by 400 is bisextile" <|
                \_ ->
                    let
                        date =
                            Date.createDate 29 2 2000
                    in
                    Expect.ok date
            ]
        , describe "Date.fromString"
            [ test "can parse a dd/mm/yyyy date format" <|
                \_ ->
                    let
                        dateString =
                            "11/11/1970"

                        expected =
                            Date.createDate 11 11 1970
                    in
                    Expect.equal expected (Date.fromString dateString)
            , test "can parse a dd/mm/yyyy date format with leading zeros" <|
                \_ ->
                    let
                        dateString =
                            "01/01/1970"

                        expected =
                            Date.createDate 1 1 1970
                    in
                    Expect.equal expected (Date.fromString dateString)
            , test "can parse a date with valid values" <|
                \_ ->
                    let
                        date =
                            Date.fromString "30/03/2012"
                    in
                    Expect.ok date
            , test "cannot parse a date with a negative day" <|
                \_ ->
                    let
                        date =
                            Date.fromString "-01/03/2012"
                    in
                    Expect.err date
            , test "cannot parse a date with a day greater than 31" <|
                \_ ->
                    let
                        date =
                            Date.fromString "32/03/2012"
                    in
                    Expect.err date
            , test "cannot parse a date with a negative month" <|
                \_ ->
                    let
                        date =
                            Date.fromString "30/-01/2012"
                    in
                    Expect.err date
            , test "cannot parse a date with a month greater than 12" <|
                \_ ->
                    let
                        date =
                            Date.fromString "30/13/2012"
                    in
                    Expect.err date
            , test "cannot parse a date with a day equal to 31 in a month of 30 days" <|
                \_ ->
                    let
                        date =
                            Date.fromString "31/04/2012"
                    in
                    Expect.err date
            , test "can parse a date with a day equal to 31 in a month of 31 days" <|
                \_ ->
                    let
                        date =
                            Date.fromString "31/01/2012"
                    in
                    Expect.ok date
            , test "cannot parse a date with a day equal 29 in february of a non-bisextile year" <|
                \_ ->
                    let
                        date =
                            Date.fromString "29/02/2011"
                    in
                    Expect.err date
            , test "can parse a date with a day equal 29 in february of a bisextile year" <|
                \_ ->
                    let
                        date =
                            Date.fromString "29/02/2004"
                    in
                    Expect.ok date
            , test "a year divisible by 4 and by 100 and not divisible by 400 is not bisextile" <|
                \_ ->
                    let
                        date =
                            Date.fromString "29/02/2100"
                    in
                    Expect.err date
            , test "a year divisible by 400 is bisextile" <|
                \_ ->
                    let
                        date =
                            Date.fromString "29/02/2000"
                    in
                    Expect.ok date
            ]
        ]
