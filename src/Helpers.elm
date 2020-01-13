module Helpers exposing (dateToLongString, monthToString, weekdayToString)

import Date
import Time


dateToLongString : Date.Date -> String
dateToLongString date =
    weekdayToString (Date.weekday date)
        ++ ", "
        ++ String.fromInt (Date.day date)
        ++ " "
        ++ monthToString (Date.month date)
        ++ " "
        ++ String.fromInt (Date.year date)


weekdayToString : Time.Weekday -> String
weekdayToString weekday =
    case weekday of
        Time.Mon ->
            "Monday"

        Time.Tue ->
            "Tuesday"

        Time.Wed ->
            "Wednesday"

        Time.Thu ->
            "Thursday"

        Time.Fri ->
            "Friday"

        Time.Sat ->
            "Saturday"

        Time.Sun ->
            "Sunday"


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "Jan."

        Time.Feb ->
            "Feb."

        Time.Mar ->
            "Mar."

        Time.Apr ->
            "Apr."

        Time.May ->
            "May"

        Time.Jul ->
            "Jul."

        Time.Jun ->
            "Jun."

        Time.Aug ->
            "Aug."

        Time.Sep ->
            "Sep."

        Time.Oct ->
            "Oct."

        Time.Nov ->
            "Nov."

        Time.Dec ->
            "Dec."
