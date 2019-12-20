module Main exposing (main)

import Browser
import Html
import Html.Events


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { statusText : String
    }


init : Model
init =
    { statusText = "Ready"
    }


type Msg
    = ClickHi


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickHi ->
            { model | statusText = "Hi!" }


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.div [] [ Html.text model.statusText ]
        , Html.div [] [ Html.button [ Html.Events.onClick ClickHi ] [ Html.text "Say hi" ] ]
        ]
