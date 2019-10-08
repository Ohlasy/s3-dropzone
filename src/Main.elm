module Main exposing (Model, Msg, filesDecoder, init, main, subscriptions, update, view)

import Browser
import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import S3
import Session exposing (Session, decodeSession, deleteSession)
import SignIn
import Upload



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
    = SignedOut SignIn.Model
    | SignedIn Session Upload.Model


type alias Flags =
    D.Value


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        decodeFlags =
            D.field "session" decodeSession
    in
    case D.decodeValue decodeFlags flags of
        Err _ ->
            ( SignedOut SignIn.init, Cmd.none )

        Ok session ->
            ( SignedIn session Upload.init, Cmd.none )



-- UPDATE


type Msg
    = SignInMsg SignIn.Msg
    | GotFiles (List File)
    | ReceiveS3Response (Result Http.Error S3.Response)
    | SignOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( SignedOut lmodel, SignInMsg lmsg ) ->
            case SignIn.update lmsg lmodel of
                ( updatedModel, cmd, Nothing ) ->
                    ( SignedOut updatedModel, Cmd.map SignInMsg cmd )

                ( _, cmd, Just session ) ->
                    ( SignedIn session Upload.init, Cmd.map SignInMsg cmd )

        ( SignedIn session jobs, GotFiles files ) ->
            let
                ( updatedJobs, startedJob ) =
                    Upload.addJobs jobs files |> Upload.startNextJob
            in
            case startedJob of
                Just job ->
                    ( SignedIn session updatedJobs, uploadFile session job.file )

                Nothing ->
                    ( SignedIn session updatedJobs, Cmd.none )

        ( SignedIn session jobs, ReceiveS3Response result ) ->
            let
                ( updatedJobs, nextJob ) =
                    Upload.finishCurrentJob jobs result |> Upload.startNextJob
            in
            case nextJob of
                Just job ->
                    ( SignedIn session updatedJobs, uploadFile session job.file )

                Nothing ->
                    ( SignedIn session updatedJobs, Cmd.none )

        ( SignedIn _ _, SignOut ) ->
            ( SignedOut SignIn.init, deleteSession )

        default ->
            ( model, Cmd.none )


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
            SignIn.viewForm |> Html.map SignInMsg

        SignedIn _ jobs ->
            div [] (List.map Upload.viewJob (Upload.allJobs jobs) ++ [ uploadForm ])


uploadForm : Html Msg
uploadForm =
    div []
        [ input
            [ type_ "file"
            , multiple True
            , on "change" (D.map GotFiles filesDecoder)
            ]
            []
        , Html.br [] []
        , button
            [ onClick SignOut ]
            [ text "Sign Out" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
