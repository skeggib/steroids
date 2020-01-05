module Date exposing (Date, createDate, fromString, getDay, getMonth, getYear, toComparable, toString)

import Parser exposing ((|.), (|=), Parser, andThen, chompWhile, end, int, problem, run, succeed, symbol)


type Date
    = Date InternalDate


type alias InternalDate =
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
            Ok (Date validDate)

        Err deadEnds ->
            case deadEnds of
                deadEnd :: _ ->
                    case deadEnd.problem of
                        Parser.Problem error ->
                            Err error

                        _ ->
                            Err ("Could not create the date '" ++ toString (Date date) ++ "'")

                _ ->
                    Err ("Could not create the date '" ++ toString (Date date) ++ "'")


parser : Parser InternalDate
parser =
    succeed InternalDate
        |. chompWhile (\c -> c == '0')
        |= int
        |. symbol "/"
        |. chompWhile (\c -> c == '0')
        |= int
        |. symbol "/"
        |= int
        |. end
        |> andThen isValid


isValid : InternalDate -> Parser InternalDate
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
            Ok (Date date)

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
    String.fromInt (getDay date) ++ "/" ++ String.fromInt (getMonth date) ++ "/" ++ String.fromInt (getYear date)


getDay : Date -> Int
getDay date =
    case date of
        Date internalDate ->
            internalDate.day


getMonth : Date -> Int
getMonth date =
    case date of
        Date internalDate ->
            internalDate.month


getYear : Date -> Int
getYear date =
    case date of
        Date internalDate ->
            internalDate.year


toComparable : Date -> Int
toComparable date =
    getYear date * 10000 + getMonth date * 100 + getDay date
