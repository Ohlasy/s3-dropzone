module Upload exposing (Model, Msg(..), Status(..), init, update, viewJob)

import Bytes exposing (Bytes)
import File exposing (File)
import Html exposing (Html, text)
import Html.Attributes exposing (href, style)
import Http
import Material.Card as Card
import Material.LayoutGrid as LayoutGrid exposing (layoutGridCell)
import S3
import SHA256
import Session exposing (Session)
import Task



-- MODEL


type Status
    = Waiting
    | Running
    | Finished (Result Http.Error S3.Response)


type alias Model =
    { file : File
    , targetFileName : Maybe String
    , session : Session
    , status : Status
    }


init : Session -> File -> Model
init session file =
    { file = file
    , targetFileName = Nothing
    , session = session
    , status = Waiting
    }



-- UPDATE


type Msg
    = Start
    | ReceiveBytes Bytes
    | ReceiveS3Response (Result Http.Error S3.Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | status = Running }, readFile model.file )

        ReceiveBytes bytes ->
            let
                originalExtension =
                    File.name model.file
                        |> splitExtension
                        |> Tuple.second

                hash =
                    hashBytes bytes

                fileName =
                    hash ++ originalExtension

                newModel =
                    { model | targetFileName = Just fileName }
            in
            ( newModel, uploadFile newModel )

        ReceiveS3Response response ->
            ( { model | status = Finished response }, Cmd.none )


uploadFile : Model -> Cmd Msg
uploadFile model =
    let
        metadata =
            { fileName = uploadFileName model
            , contentType = File.mime model.file
            , file = model.file
            }

        config =
            s3Config model.session
    in
    S3.uploadFile metadata config ReceiveS3Response


uploadFileName : Model -> String
uploadFileName model =
    Maybe.withDefault (File.name model.file) model.targetFileName


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



-- FILE UTILS


readFile : File -> Cmd Msg
readFile file =
    Task.perform ReceiveBytes (File.toBytes file)


hashBytes : Bytes -> String
hashBytes =
    SHA256.fromBytes
        >> SHA256.toHex
        >> String.left 8


splitExtension : String -> ( String, String )
splitExtension path =
    case String.reverse path |> String.split extSeparator of
        [] ->
            ( "", "" )

        [ a ] ->
            ( String.reverse a, "" )

        x :: xs ->
            ( String.reverse <| String.join extSeparator xs, extSeparator ++ String.reverse x )


extSeparator : String
extSeparator =
    "."



-- VIEW


viewJob : Model -> Html a
viewJob job =
    layoutGridCell [ LayoutGrid.span2Desktop ]
        [ Card.card Card.cardConfig
            { blocks =
                [ Card.cardMedia wideAspectCardConfig (coverImageForJob job)
                , Card.cardBlock <|
                    Html.div [ style "padding" "10px" ]
                        [ Html.h2 [] [ text (File.name job.file) ]
                        , Html.p [] [ viewStatus job ]
                        ]
                ]
            , actions = Nothing
            }
        ]


wideAspectCardConfig : Card.CardMediaConfig a
wideAspectCardConfig =
    let
        defaultConfig =
            Card.cardMediaConfig
    in
    { defaultConfig | aspect = Just Card.SixteenToNine }


coverImageForJob : Model -> String
coverImageForJob job =
    case job.status of
        Finished (Ok { location }) ->
            location

        _ ->
            "placeholder.jpg"


viewStatus : Model -> Html a
viewStatus model =
    case model.status of
        Waiting ->
            Html.text "Waiting…"

        Running ->
            Html.text "Uploading…"

        Finished (Ok _) ->
            let
                location =
                    Session.targetUrlForFile (uploadFileName model) model.session
            in
            Html.a [ href location ] [ Html.text "Uploaded" ]

        Finished (Err e) ->
            Html.text ("Failed: " ++ Debug.toString e)
