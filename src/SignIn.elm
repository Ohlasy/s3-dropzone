module SignIn exposing (Model, Msg, init, update, viewForm)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Session exposing (Session, encodeSession, saveSession)


type alias Form =
    { accessKey : String
    , secretKey : String
    }


type Model
    = FillingForm Form
    | SignedIn Session


type Msg
    = UpdateAccessKey String
    | UpdateSecretKey String
    | SubmitForm


init : Model
init =
    FillingForm { accessKey = "", secretKey = "" }


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Session )
update msg model =
    case ( model, msg ) of
        ( FillingForm form, UpdateAccessKey s ) ->
            ( FillingForm { form | accessKey = s }, Cmd.none, Nothing )

        ( FillingForm form, UpdateSecretKey s ) ->
            ( FillingForm { form | secretKey = s }, Cmd.none, Nothing )

        ( FillingForm form, SubmitForm ) ->
            ( SignedIn form, form |> encodeSession |> saveSession, Just form )

        default ->
            ( model, Cmd.none, Nothing )


viewForm : Html Msg
viewForm =
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
            , onClick SubmitForm
            ]
            []
        ]
