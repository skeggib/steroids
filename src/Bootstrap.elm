module Bootstrap exposing (col, row)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


row : List (Html.Attribute msg) -> List (Html msg) -> Html msg
row attributes elements =
    div (class "row" :: attributes) elements


col : List (Html.Attribute msg) -> Html msg -> Html msg
col attributes element =
    div (class "col" :: attributes) [ element ]
