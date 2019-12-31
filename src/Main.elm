module Main exposing (Model, Msg, filesDecoder, init, main, subscriptions, update, view)

import Browser
import File exposing (File)
import Html exposing (Html)
import Html.Attributes as Atts
import Html.Events as Events
import Json.Decode as JSON
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Material.LayoutGrid exposing (layoutGrid, layoutGridInner)
import Material.TopAppBar as TopAppBar exposing (topAppBar, topAppBarConfig)
import Material.Typography as Typography
import Queue exposing (Queue)
import Session exposing (Session, decodeSession, deleteSession)
import SignIn
import Upload



-- MAIN


main : Program Flags Model Msg
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
    | SignedIn Session Queue


type alias Flags =
    JSON.Value


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        decodeFlags =
            JSON.field "session" decodeSession
    in
    case JSON.decodeValue decodeFlags flags of
        Err _ ->
            ( SignedOut SignIn.init, Cmd.none )

        Ok session ->
            ( SignedIn session Queue.init, Cmd.none )



-- UPDATE


type Msg
    = SignInMsg SignIn.Msg
    | UpdateUpload Upload.Msg
    | GotFiles (List File)
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
                    ( SignedIn session Queue.init, Cmd.map SignInMsg cmd )

        -- Add new uploads to the queue
        ( SignedIn session queue, GotFiles files ) ->
            Queue.addJobs queue session files
                |> Queue.update
                |> Tuple.mapFirst (SignedIn session)
                |> Tuple.mapSecond (Cmd.map UpdateUpload)

        -- Update queue
        ( SignedIn session queue, UpdateUpload umsg ) ->
            let
                ( queue1, cmd1 ) =
                    Queue.updateCurrentJob queue umsg

                ( queue2, cmd2 ) =
                    Queue.update queue1

                cmd =
                    Cmd.batch [ cmd1, cmd2 ]
            in
            ( SignedIn session queue2, cmd |> Cmd.map UpdateUpload )

        -- Sign Out
        ( SignedIn _ _, SignOut ) ->
            ( SignedOut SignIn.init, deleteSession )

        _ ->
            ( model, Cmd.none )


filesDecoder : JSON.Decoder (List File)
filesDecoder =
    JSON.at [ "target", "files" ] (JSON.list File.decoder)



-- VIEW


view : Model -> Html Msg
view model =
    Html.div [ Typography.typography ]
        [ viewTopAppBar model
        , Html.div [ TopAppBar.fixedAdjust ]
            [ viewContent model ]
        ]


viewTopAppBar : Model -> Html Msg
viewTopAppBar model =
    let
        appName =
            "S3 Dropzone"

        heading =
            case model of
                SignedOut _ ->
                    appName

                SignedIn session _ ->
                    appName ++ " | " ++ session.bucket
    in
    topAppBar topAppBarConfig
        [ TopAppBar.row []
            [ TopAppBar.section [ TopAppBar.alignStart ]
                [ Html.span [ TopAppBar.title ] [ Html.text heading ] ]
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
            Html.div []
                [ layoutGrid []
                    [ layoutGridInner [] (List.map Upload.viewJob (Queue.allJobs jobs))
                    ]
                , uploadForm
                ]


uploadForm : Html Msg
uploadForm =
    Html.div []
        [ Html.input
            [ Atts.type_ "file"
            , Atts.multiple True
            , Events.on "change" (JSON.map GotFiles filesDecoder)
            ]
            []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
