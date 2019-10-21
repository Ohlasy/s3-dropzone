module Main exposing (Model, Msg, filesDecoder, init, main, subscriptions, update, view)

import Browser
import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Material.LayoutGrid as LayoutGrid exposing (layoutGrid, layoutGridCell, layoutGridInner)
import Material.TopAppBar as TopAppBar exposing (topAppBar, topAppBarConfig)
import Material.Typography as Typography
import Queue exposing (Queue)
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
    | SignedIn Session Queue


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

        default ->
            ( model, Cmd.none )


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
                    [ layoutGridInner [] (List.map Upload.viewJob (Queue.allJobs jobs))
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
