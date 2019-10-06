module Main exposing (Model, Msg(..), filesDecoder, init, main, subscriptions, update, view)

import Browser
import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import S3



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = SelectingFile
    | UploadingFile File
    | UploadFinished (Result Http.Error S3.Response)


init : () -> ( Model, Cmd Msg )
init _ =
    ( SelectingFile, Cmd.none )



-- UPDATE


type Msg
    = GotFiles (List File)
    | ReceiveS3Response (Result Http.Error S3.Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFiles files ->
            case List.head files of
                Just file ->
                    ( UploadingFile file, uploadFile file )

                Nothing ->
                    ( SelectingFile, Cmd.none )

        ReceiveS3Response result ->
            ( UploadFinished result, Cmd.none )


s3Config : S3.Config
s3Config =
    S3.config
        { accessKey = "…"
        , secretKey = "…"
        , bucket = "test-ohlasy-info"
        , region = "eu-central-1"
        }
        |> S3.withAwsS3Host "s3.eu-central-1.amazonaws.com"


uploadFile : File -> Cmd Msg
uploadFile file =
    let
        metadata =
            { fileName = File.name file
            , contentType = File.mime file
            , file = file
            }
    in
    S3.uploadFile metadata s3Config ReceiveS3Response


filesDecoder : D.Decoder (List File)
filesDecoder =
    D.at [ "target", "files" ] (D.list File.decoder)



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        SelectingFile ->
            uploadForm

        UploadingFile file ->
            Html.text ("Uploading “" ++ File.name file ++ "”…")

        UploadFinished result ->
            case result of
                Err e ->
                    Html.text ("Error: " ++ Debug.toString e)

                Ok { location } ->
                    Html.a [ href location ]
                        [ Html.text "Uploaded!" ]


uploadForm : Html Msg
uploadForm =
    div []
        [ input
            [ type_ "file"
            , multiple False
            , on "change" (D.map GotFiles filesDecoder)
            ]
            []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
