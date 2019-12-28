module Exercise exposing (Exercise)

import Date


type alias Exercise =
    { name : String
    , setsNumber : Int
    , repetitionsNumber : Int
    , date : Date.Date
    }
