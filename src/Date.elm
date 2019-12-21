module Date exposing (Date, createDate, fromString, toString)

import Parser exposing ((|.), (|=), Parser, andThen, end, int, problem, run, succeed, symbol, chompIf, chompWhile, oneOf)


type alias Date =
    { day : Int
    , month : Int
    , year : Int
    }


createDate : Int -> Int -> Int -> Result String Date
createDate day month year =
    let
        date =
            { day = day
            , month = month
            , year = year
            }
    in
    case run (isValid date) "" of
        Ok validDate ->
            Ok validDate

        Err deadEnds ->
            case deadEnds of
                deadEnd :: _ ->
                    case deadEnd.problem of
                        Parser.Problem error ->
                            Err error

                        _ ->
                            Err ("Could not create the date '" ++ toString date ++ "'")

                _ ->
                    Err ("Could not create the date '" ++ toString date ++ "'")


parser : Parser Date
parser =
    succeed Date
        |. chompWhile (\c -> c == '0')
        |= int
        |. symbol "/"
        |. chompWhile (\c -> c == '0')
        |= int
        |. symbol "/"
        |= int
        |. end
        |> andThen isValid


isValid : Date -> Parser Date
isValid date =
    if date.month < 1 || date.month > 12 then
        problem "The month must be between 1 and 12"

    else if (date.month == 1 || date.month == 3 || date.month == 5 || date.month == 7 || date.month == 8 || date.month == 10 || date.month == 12) && (date.day < 1 || date.day > 31) then
        problem "The day must be between 1 and 31"

    else if (date.month == 4 || date.month == 6 || date.month == 9 || date.month == 11) && (date.day < 1 || date.day > 30) then
        problem "The day must be between 1 and 30"

    else if date.month == 2 && (remainderBy 4 date.year == 0 && remainderBy 100 date.year /= 0 || remainderBy 400 date.year == 0) && (date.day < 1 || date.day > 29) then
        problem "The day must be between 1 and 29"

    else if date.month == 2 && (remainderBy 100 date.year == 0 && remainderBy 400 date.year /= 0 || remainderBy 4 date.year /= 0) && (date.day < 1 || date.day > 28) then
        problem "The day must be between 1 and 28"

    else
        succeed date


fromString : String -> Result String Date
fromString string =
    case run parser string of
        Ok date ->
            Ok date

        Err deadEnds ->
            case deadEnds of
                deadEnd :: _ ->
                    case deadEnd.problem of
                        Parser.Problem error ->
                            Err error

                        _ ->
                            Err "A date should be of the form dd/mm/yyyy"

                _ ->
                    Err "A date should be of the form dd/mm/yyyy"


toString : Date -> String
toString date =
    String.fromInt date.day ++ "/" ++ String.fromInt date.month ++ "/" ++ String.fromInt date.year
