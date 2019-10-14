module Main exposing (Model, Msg, filesDecoder, init, main, subscriptions, update, view)

import Browser
import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Material.LayoutGrid as LayoutGrid exposing (layoutGrid, layoutGridCell, layoutGridInner)
import Material.TopAppBar as TopAppBar exposing (topAppBar, topAppBarConfig)
import Material.Typography as Typography
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
        -- Handle sign-in message, possibly switching to upload
        ( SignedOut lmodel, SignInMsg lmsg ) ->
            case SignIn.update lmsg lmodel of
                ( updatedModel, cmd, Nothing ) ->
                    ( SignedOut updatedModel, Cmd.map SignInMsg cmd )

                ( _, cmd, Just session ) ->
                    ( SignedIn session Upload.init, Cmd.map SignInMsg cmd )

        -- Add new uploads to the queue
        ( SignedIn session jobs, GotFiles files ) ->
            updateJobQueue session jobs (Upload.addJobs files)

        -- Finish current upload
        ( SignedIn session jobs, ReceiveS3Response result ) ->
            updateJobQueue session jobs (Upload.finishCurrentJob result)

        -- Sign Out
        ( SignedIn _ _, SignOut ) ->
            ( SignedOut SignIn.init, deleteSession )

        default ->
            ( model, Cmd.none )


updateJobQueue : Session -> Upload.Model -> (Upload.Model -> Upload.Model) -> ( Model, Cmd Msg )
updateJobQueue session queue action =
    let
        ( updatedQueue, nextJob ) =
            queue |> action |> Upload.startNextJob

        updatedModel =
            SignedIn session updatedQueue
    in
    case nextJob of
        Just job ->
            ( updatedModel, uploadFile session job.file )

        Nothing ->
            ( updatedModel, Cmd.none )


s3Config : Session -> S3.Config
s3Config session =
    let
        awsHost =
            String.join "." [ "s3", session.region, "amazonaws.com" ]
    in
    S3.config
        { accessKey = session.accessKey
        , secretKey = session.secretKey
        , bucket = session.bucket
        , region = session.region
        }
        |> S3.withAwsS3Host awsHost
        |> S3.withPrefix session.folderPrefix


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
    Html.div [ Typography.typography ]
        [ viewTopAppBar
        , Html.div [ TopAppBar.fixedAdjust ]
            [ viewContent model ]
        ]


viewTopAppBar : Html Msg
viewTopAppBar =
    topAppBar topAppBarConfig
        [ TopAppBar.row []
            [ TopAppBar.section [ TopAppBar.alignStart ]
                [ Html.span [ TopAppBar.title ] [ text "S3 Dropzone" ] ]
            , TopAppBar.section [ TopAppBar.alignEnd ]
                [ iconButton { iconButtonConfig | onClick = Just SignOut, label = Just "Sign Out" } "exit_to_app" ]
            ]
        ]


viewContent : Model -> Html Msg
viewContent model =
    case model of
        SignedOut _ ->
            SignIn.viewForm |> Html.map SignInMsg

        SignedIn _ jobs ->
            div []
                [ layoutGrid []
                    [ layoutGridInner [] (List.map Upload.viewJob (Upload.allJobs jobs))
                    ]
                , uploadForm
                ]


uploadForm : Html Msg
uploadForm =
    div []
        [ input
            [ type_ "file"
            , multiple True
            , on "change" (D.map GotFiles filesDecoder)
            ]
            []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
