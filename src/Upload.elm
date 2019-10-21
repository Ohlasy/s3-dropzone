module Upload exposing (..)

import File exposing (File)
import Html exposing (Html, text)
import Html.Attributes exposing (href, style)
import Http
import Material.Card exposing (..)
import Material.Icon exposing (icon, iconConfig)
import Material.LayoutGrid as LayoutGrid exposing (layoutGridCell)
import Material.TextField exposing (textField, textFieldConfig)
import Material.Typography as Typography
import S3
import Session exposing (Session)



-- MODEL


type Status
    = Waiting
    | Running
    | Finished (Result Http.Error S3.Response)


type alias Model =
    { file : File
    , session : Session
    , status : Status
    }


init : Session -> File -> Model
init session file =
    Model file session Waiting



-- UPDATE


type Msg
    = Start
    | ReceiveS3Response (Result Http.Error S3.Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | status = Running }, uploadFile model.session model.file )

        ReceiveS3Response response ->
            ( { model | status = Finished response }, Cmd.none )


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



-- VIEW


viewJob : Model -> Html a
viewJob job =
    layoutGridCell [ LayoutGrid.span2Desktop ]
        [ card cardConfig
            { blocks =
                [ cardMedia { cardMediaConfig | aspect = Just SixteenToNine } (coverImageForJob job)
                , cardBlock <|
                    Html.div [ style "padding" "10px" ]
                        [ Html.h2 [] [ text (File.name job.file) ]
                        , Html.p [] [ viewStatus job.status ]
                        ]
                ]
            , actions = Nothing
            }
        ]


coverImageForJob : Model -> String
coverImageForJob job =
    case job.status of
        Finished (Ok { location }) ->
            location

        default ->
            "placeholder.jpg"


viewStatus : Status -> Html a
viewStatus s =
    case s of
        Waiting ->
            Html.text "Waiting…"

        Running ->
            Html.text "Uploading…"

        Finished (Ok { location }) ->
            Html.a [ href location ] [ Html.text "Uploaded" ]

        Finished (Err e) ->
            Html.text ("Failed: " ++ Debug.toString e)
