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


type alias Session =
    { accessKey : String
    , secretKey : String
    }


type alias SignInFormModel =
    { accessKey : String
    , secretKey : String
    }


type UploadModel
    = SelectingFile
    | UploadingFile File
    | UploadFinished (Result Http.Error S3.Response)


type Model
    = SignedOut SignInFormModel
    | SignedIn Session UploadModel
    | InvalidState String


init : () -> ( Model, Cmd Msg )
init _ =
    ( SignedOut { accessKey = "", secretKey = "" }, Cmd.none )



-- UPDATE


type Msg
    = UpdateAccessKey String
    | UpdateSecretKey String
    | SignIn
    | GotFiles (List File)
    | ReceiveS3Response (Result Http.Error S3.Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( SignedOut form, UpdateAccessKey s ) ->
            ( SignedOut { form | accessKey = s }, Cmd.none )

        ( SignedOut form, UpdateSecretKey s ) ->
            ( SignedOut { form | secretKey = s }, Cmd.none )

        ( SignedOut form, SignIn ) ->
            ( SignedIn form SelectingFile, Cmd.none )

        ( SignedIn session SelectingFile, GotFiles files ) ->
            case List.head files of
                Just file ->
                    ( SignedIn session (UploadingFile file), uploadFile session file )

                Nothing ->
                    ( SignedIn session SelectingFile, Cmd.none )

        ( SignedIn session (UploadingFile _), ReceiveS3Response result ) ->
            ( SignedIn session (UploadFinished result), Cmd.none )

        default ->
            ( InvalidState (Debug.toString model), Cmd.none )


s3Config : Session -> S3.Config
s3Config session =
    S3.config
        { accessKey = session.accessKey
        , secretKey = session.secretKey
        , bucket = "test-ohlasy-info"
        , region = "eu-central-1"
        }
        |> S3.withAwsS3Host "s3.eu-central-1.amazonaws.com"
        |> S3.withPrefix "test"


uploadFile : Session -> File -> Cmd Msg
uploadFile session file =
    let
        metadata =
            { fileName = File.name file
            , contentType = File.mime file
            , file = file
            }

        config =
            s3Config session
    in
    S3.uploadFile metadata config ReceiveS3Response


filesDecoder : D.Decoder (List File)
filesDecoder =
    D.at [ "target", "files" ] (D.list File.decoder)



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        SignedOut _ ->
            signInForm

        SignedIn _ SelectingFile ->
            uploadForm

        SignedIn _ (UploadingFile file) ->
            Html.text ("Uploading “" ++ File.name file ++ "”…")

        SignedIn _ (UploadFinished result) ->
            case result of
                Err e ->
                    Html.text ("Error: " ++ Debug.toString e)

                Ok { location } ->
                    Html.a [ href location ]
                        [ Html.text "Uploaded!" ]

        InvalidState s ->
            Html.text ("Invalid state: " ++ s)


signInForm : Html Msg
signInForm =
    div []
        [ input
            [ type_ "text"
            , onInput UpdateAccessKey
            , placeholder "access key"
            ]
            []
        , input
            [ type_ "password"
            , onInput UpdateSecretKey
            , placeholder "secret key"
            ]
            []
        , input
            [ type_ "submit"
            , value "Sign In"
            , onClick SignIn
            ]
            []
        ]


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
