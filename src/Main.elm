module Main exposing (main)

import Bootstrap
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Regex
import Validate exposing (Validator)



-- ------------------
-- MODEL
-- ------------------


type alias Model =
    { secret : String
    , name : String
    , ip : String
    , validationErrors : ValidationErrors
    , asyncStatus : AsyncStatus
    }


blankModel : Model
blankModel =
    { secret = ""
    , name = ""
    , ip = ""
    , validationErrors = emptyErrors
    , asyncStatus = Waiting
    }


type alias ValidationErrors =
    { ip : List String
    }


emptyErrors : ValidationErrors
emptyErrors =
    { ip = []
    }


type AsyncStatus
    = Waiting
    | EvaSubmitting
    | EvaSuccess
    | EvaError String



-- ------------------
-- INIT
-- ------------------


init : String -> ( Model, Cmd Msg )
init secret =
    ( { blankModel | secret = secret }, Cmd.none )



-- ------------------
-- UPDATE
-- ------------------


type Msg
    = UpdateName String
    | UpdateIp String
    | OnBlurIp
    | Submit
    | ResponseReceived (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName newName ->
            ( { model | name = newName }, Cmd.none )

        UpdateIp newIp ->
            ( { model | ip = newIp }, Cmd.none )

        OnBlurIp ->
            let
                oldErrors =
                    model.validationErrors

                newIpErrors =
                    case Validate.validate ipValidator model.ip of
                        Err errors ->
                            errors

                        Ok _ ->
                            []

                newErrors =
                    { oldErrors | ip = newIpErrors }
            in
            ( { model | validationErrors = newErrors }, Cmd.none )

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



-- View


view : Model -> Html Msg
view model =
    let
        nameErrors =
            case Validate.validate nameValidator model.name of
                Ok _ ->
                    []

                Err errs ->
                    errs

        viewErrors errs =
            case errs of
                [] ->
                    p [ class "error-container" ] []

                errors ->
                    errors
                        |> List.map (\err -> div [] [ text err ])
                        |> p [ class "error-container", style "background-color" "pink" ]
    in
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
                    , viewErrors nameErrors
                    , div [ class "form-group" ]
                        [ label [ for "ip" ] [ text "IP Address" ]
                        , input
                            [ onInput UpdateIp
                            , onBlur OnBlurIp
                            , class "form-control"
                            , id "ip"
                            , value model.ip
                            , autocomplete False
                            ]
                            []
                        ]
                    , viewErrors model.validationErrors.ip
                    , button
                        [ onClick Submit
                        , class "btn btn-primary"
                        , disabled <| model.name == "" || model.ip == ""
                        ]
                        [ text "GO!" ]
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
                            , a [ href url, target "_blank" ] [ text <| mkSpecialUrl model ]
                            , text " in 5 minutes"
                            ]

                    EvaError err ->
                        div [ class "bg-warning" ]
                            [ h3 [] [ text "Whoops - an error has occurred" ]
                            , text <| "Please contact Stephen... mentioning " ++ err
                            ]
                ]
            ]
        ]


mkSpecialUrl model =
    "http://specialurl/" ++ model.name



-- ------------------
-- VALIDATION
-- ------------------


nameValidator : Validator String String
nameValidator =
    Validate.all
        [ Validate.ifFalse isValidName "A valid name uses only alpha-numeric characters"
        ]


ipValidator : Validator String String
ipValidator =
    Validate.all
        [ isValidIPAddress
        , Validate.ifBlank identity "You need to provide an IP address!"
        ]


isValidIPAddress : Validator String String
isValidIPAddress =
    Validate.ifFalse isValidIP "Not a valid IP address"


isValidIP : String -> Bool
isValidIP string =
    case Regex.fromString ipRegex of
        Just regex ->
            Regex.contains regex string

        Nothing ->
            False


ipRegex : String
ipRegex =
    """\\b((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(\\.|$)){4}\\b"""


isValidName : String -> Bool
isValidName string =
    case Regex.fromString nameRegex of
        Just regex ->
            Regex.contains regex string

        Nothing ->
            False


nameRegex : String
nameRegex =
    """^[a-zA-Z0-9]*$"""



-- ------------------
-- HTTP
-- ------------------


postUrl : String
postUrl =
    "http://localhost:3000/submit"


sendToServer : Model -> Cmd Msg
sendToServer model =
    let
        url =
            postUrl ++ "?" ++ "IP=" ++ model.ip ++ "&" ++ "NAME=" ++ model.name

        myRequest =
            { method = "POST"
            , headers = [ Http.header "Authorization" ("Bearer: " ++ model.secret) ]
            , url = url
            , body = Http.emptyBody
            , expect = Http.expectWhatever ResponseReceived
            , timeout = Nothing
            , tracker = Nothing
            }
    in
    Http.request myRequest



-- ------------------
-- MAIN
-- ------------------


main : Program String Model Msg
main =
    Browser.document
        { init = init
        , view = \m -> { title = "Eva", body = [ view m ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }
