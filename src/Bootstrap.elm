module Bootstrap exposing (inputWithLabel, passwordWithLabel)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


inputWithButton : (String -> msg) -> msg -> String -> Html msg
inputWithButton updater clicker val =
    div [ class "input-group" ]
        [ input
            [ onInput updater
            , class "form-control"
            , value val
            ]
            []
        , span
            [ class "input-group-btn" ]
            [ button
                [ class "btn btn-primary", onClick clicker ]
                [ text "Go!" ]
            ]
        ]


inputWithLabel : (String -> msg) -> String -> String -> String -> Html msg
inputWithLabel msg lab id_ val =
    div [ class "form-group" ]
        [ label [ for id_ ] [ text lab ]
        , input
            [ onInput msg
            , class "form-control"
            , id id_
            , value val
            ]
            []
        ]


passwordWithLabel : (String -> msg) -> String -> String -> String -> Html msg
passwordWithLabel msg lab id_ val =
    div [ class "form-group" ]
        [ label [ for id_ ] [ text lab ]
        , input
            [ onInput msg
            , type_ "password"
            , class "form-control"
            , id id_
            , value val
            ]
            []
        ]
