module Bootstrap exposing (ButtonStyle(..), buttonHyperlink, col, row)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


row : List (Html.Attribute msg) -> List (Html msg) -> Html msg
row attributes elements =
    div (class "row" :: attributes) elements


col : List (Html.Attribute msg) -> Html msg -> Html msg
col attributes element =
    div (class "col" :: attributes) [ element ]


buttonHyperlink : ButtonStyle -> List (Html.Attribute msg) -> String -> String -> Html msg
buttonHyperlink style attributes href text =
    Html.a
        (attributes |> List.append (buttonCss style) |> List.append [ Html.Attributes.href href ])
        [ Html.text text ]


type ButtonStyle
    = Primary
    | Secondary
    | Success
    | Danger
    | Warning
    | Info
    | Light
    | Dark
    | Link


buttonCss : ButtonStyle -> List (Html.Attribute msg)
buttonCss style =
    case style of
        Primary ->
            [ Html.Attributes.class "btn", Html.Attributes.class "btn-primary" ]

        Secondary ->
            [ Html.Attributes.class "btn", Html.Attributes.class "btn-secondary" ]

        Success ->
            [ Html.Attributes.class "btn", Html.Attributes.class "btn-success" ]

        Danger ->
            [ Html.Attributes.class "btn", Html.Attributes.class "btn-danger" ]

        Warning ->
            [ Html.Attributes.class "btn", Html.Attributes.class "btn-warning" ]

        Info ->
            [ Html.Attributes.class "btn", Html.Attributes.class "btn-info" ]

        Light ->
            [ Html.Attributes.class "btn", Html.Attributes.class "btn-light" ]

        Dark ->
            [ Html.Attributes.class "btn", Html.Attributes.class "btn-dark" ]

        Link ->
            [ Html.Attributes.class "btn", Html.Attributes.class "btn-link" ]
