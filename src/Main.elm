module Main exposing (main)

import Bootstrap
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Validate



-- Model


type alias Model =
    { name : String
    , ip : String
    , validationErrors : ValidationErrors
    , asyncStatus : AsyncStatus
    }


blankModel : Model
blankModel =
    { name = ""
    , ip = ""
    , validationErrors = emptyErrors
    , asyncStatus = Waiting
    }


type alias ValidationErrors =
    { name : List String
    , ip : List String
    }


type AsyncStatus
    = Waiting
    | EvaSubmitting
    | EvaSuccess
    | EvaError String



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    ( blankModel, Cmd.none )



-- Update


type Msg
    = UpdateName String
    | UpdateIp String
    | Submit
    | ResponseReceived (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName newName ->
            ( { model | name = newName }, Cmd.none )

        UpdateIp newIp ->
            let
                oldErrors =
                    model.validationErrors

                newIpErrors =
                    case Validate.validate ipValidator newIp of
                        Err errors ->
                            errors

                        Ok _ ->
                            []

                newErrors =
                    { oldErrors | ip = newIpErrors }
            in
            ( { model | ip = newIp, validationErrors = newErrors }, Cmd.none )

        Submit ->
            case Validate.validate ipValidator model.ip of
                Err errors ->
                    let
                        oldErrors =
                            model.validationErrors

                        newErrors =
                            { oldErrors | ip = errors }
                    in
                    ( { model | validationErrors = newErrors }, Cmd.none )

                Ok _ ->
                    let
                        newModel =
                            { model | asyncStatus = EvaSubmitting }
                    in
                    ( newModel, sendToServer newModel )

        ResponseReceived res ->
            case res of
                Ok _ ->
                    ( { model | asyncStatus = EvaSuccess }, Cmd.none )

                Err err ->
                    ( { model | asyncStatus = EvaError <| stringFromHttpError err }, Cmd.none )


stringFromHttpError : Http.Error -> String
stringFromHttpError err =
    case err of
        BadUrl str ->
            "BadUrl " ++ str

        Timeout ->
            "Timeout"

        NetworkError ->
            "NetworkError"

        BadStatus res ->
            "BadStatus " ++ String.fromInt res

        BadBody str ->
            "BadBody " ++ str


emptyErrors : ValidationErrors
emptyErrors =
    { name = []
    , ip = []
    }



-- View


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "d-flex flex-column justify-content-center" ]
            [ h1 [] [ text "EVA" ]
            , p []
                [ text "This is "
                , strong [] [ text "EVA " ]
                , text "- Easy VPN Avoider."
                ]
            , div [ class "card" ]
                [ div [ class "card-body" ]
                    [ Bootstrap.inputWithLabel UpdateName "Name" "name" model.name
                    , Bootstrap.inputWithLabel UpdateIp "IP address" "ip" model.ip
                    , case model.validationErrors.ip of
                        [] ->
                            text ""

                        errors ->
                            errors
                                |> List.map text
                                |> p [ style "background-color" "pink" ]
                    , button [ onClick Submit, class "btn btn-primary" ] [ text "GO!" ]
                    ]
                ]
            , div []
                [ case model.asyncStatus of
                    Waiting ->
                        text ""

                    EvaSubmitting ->
                        text "In progress..."

                    EvaSuccess ->
                        let
                            url =
                                mkSpecialUrl model
                        in
                        div [ class "text-success" ]
                            [ text "go check "
                            , a [ href url ] [ text url ]
                            , text " in 5 minutes"
                            ]

                    EvaError err ->
                        div [ class "bg-warning" ] [ text err ]
                ]
            ]
        ]


mkSpecialUrl model =
    "http://specialurl/"


ipValidator =
    Validate.all
        [ Validate.ifBlank identity "You need to provide an IP address!"
        ]


postUrl : String
postUrl =
    "http://localhost:3000/submit"


sendToServer : Model -> Cmd Msg
sendToServer model =
    Http.post
        { url = postUrl ++ "?" ++ "IP=" ++ model.ip ++ "&" ++ "NAME=" ++ model.name
        , body = Http.emptyBody
        , expect = Http.expectWhatever ResponseReceived
        }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = \m -> { title = "Eva", body = [ view m ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }
