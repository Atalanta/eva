module HomePage exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)
import Validate


type alias ServerResponse =
    ()


type alias Model =
    { name : String
    , ip : String
    , errors : Errors
    , saveRequest : WebData ServerResponse
    }


type alias Errors =
    { name : List String
    , ip : List String
    }


type Msg
    = UpdateName String
    | UpdateIp String
    | Submit
    | ResponseReceived (WebData ServerResponse)


emptyErrors : Errors
emptyErrors =
    { name = []
    , ip = []
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { name = ""
      , ip = ""
      , saveRequest = NotAsked
      , errors = emptyErrors
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div [ class "jumbotron" ]
        [ h1 [] [ text "EVA" ]
        , p []
            [ text "This is "
            , strong [] [ text "EVA " ]
            , text <|
                """
                 - Easy VPN Avoider.
                """
            ]
        , div []
            [ label [ for "name" ] [ text "Name" ]
            , input [ name "name", id "name", value model.name, onInput UpdateName ] []
            ]
        , div []
            [ label [ for "ip" ] [ text "Ip" ]
            , input [ name "ip", id "ip", value model.ip, onInput UpdateIp ] []
            , case model.errors.ip of
                [] ->
                    text ""

                errors ->
                    p [ style "background-color" "pink" ] <|
                        List.map text <|
                            errors
            ]
        , button [ onClick Submit ] [ text "GO!" ]
        , div [] [ text "Model content:" ]
        , div [] [ text ("name: " ++ model.name) ]
        , div [] [ text ("ip: " ++ model.ip) ]
        , div [] [ text "Submission result" ]
        , div []
            [ case model.saveRequest of
                NotAsked ->
                    text "Not yet requested!"

                Loading ->
                    text "In progress..."

                Success _ ->
                    text "Ok!"

                Failure _ ->
                    text "Error =("
            ]
        ]


ipValidator =
    Validate.all
        [ Validate.ifBlank identity "You need to provide an IP address!"
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName newName ->
            ( { model | name = newName }, Cmd.none )

        UpdateIp newIp ->
            let
                oldErrors =
                    model.errors

                newIpErrors =
                    case Validate.validate ipValidator newIp of
                        Err errors ->
                            errors

                        Ok _ ->
                            []

                newErrors =
                    { oldErrors | ip = newIpErrors }
            in
            ( { model | ip = newIp, errors = newErrors }, Cmd.none )

        Submit ->
            case Validate.validate ipValidator model.ip of
                Err errors ->
                    let
                        oldErrors =
                            model.errors

                        newErrors =
                            { oldErrors | ip = errors }
                    in
                    ( { model | errors = newErrors }, Cmd.none )

                Ok _ ->
                    ( { model | saveRequest = Loading }, sendToServer model )

        ResponseReceived response ->
            ( { model | saveRequest = response }, Cmd.none )


postUrl : String
postUrl =
    "https://my.api.com/submit"


sendToServer : Model -> Cmd Msg
sendToServer model =
    Http.post
        { url = postUrl ++ "?" ++ "IP=" ++ model.ip ++ "&" ++ "NAME=" ++ model.name
        , body = Http.emptyBody
        , expect = Http.expectWhatever (RemoteData.fromResult >> ResponseReceived)
        }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
