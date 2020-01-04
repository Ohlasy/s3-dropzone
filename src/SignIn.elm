module SignIn exposing (Model, Msg, getDefaultConfig, init, update, viewForm)

import Html exposing (Html)
import Html.Attributes
import Http
import Json.Decode as JSON
import Material.Button exposing (buttonConfig, textButton)
import Material.Dialog exposing (dialog, dialogConfig)
import Material.TextField exposing (textField, textFieldConfig)
import Session exposing (Session, encodeSession, saveSession)


type alias Model =
    { accessKey : String
    , secretKey : String
    , bucket : String
    , folderPrefix : String
    , publicUrlPrefix : String
    }


type Msg
    = ReceiveDefaultConfig (Result Http.Error DefaultConfig)
    | UpdateAccessKey String
    | UpdateSecretKey String
    | UpdateBucket String
    | UpdateFolderPrefix String
    | UpdatePublicUrlPrefix String
    | SubmitForm


init : Model
init =
    { accessKey = ""
    , secretKey = ""
    , bucket = ""
    , folderPrefix = ""
    , publicUrlPrefix = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Session )
update msg form =
    case msg of
        ReceiveDefaultConfig result ->
            let
                noDefaultConfig =
                    DefaultConfig Nothing Nothing Nothing

                config =
                    Result.withDefault noDefaultConfig result

                prefilledForm =
                    { form
                        | bucket = Maybe.withDefault form.bucket config.bucket
                        , folderPrefix = Maybe.withDefault form.folderPrefix config.folderPrefix
                        , publicUrlPrefix = Maybe.withDefault form.publicUrlPrefix config.publicUrlPrefix
                    }
            in
            ( prefilledForm, Cmd.none, Nothing )

        UpdateAccessKey s ->
            ( { form | accessKey = s }, Cmd.none, Nothing )

        UpdateSecretKey s ->
            ( { form | secretKey = s }, Cmd.none, Nothing )

        UpdateBucket s ->
            ( { form | bucket = s }, Cmd.none, Nothing )

        UpdateFolderPrefix s ->
            ( { form | folderPrefix = s }, Cmd.none, Nothing )

        UpdatePublicUrlPrefix s ->
            ( { form | publicUrlPrefix = s }, Cmd.none, Nothing )

        SubmitForm ->
            let
                session =
                    makeSession form

                saveCmd =
                    session |> encodeSession |> saveSession
            in
            ( form, saveCmd, Just session )


makeSession : Model -> Session
makeSession form =
    { accessKey = form.accessKey
    , secretKey = form.secretKey
    , bucket = form.bucket
    , region = "eu-central-1"
    , publicUrlPrefix = form.publicUrlPrefix
    , folderPrefix = form.folderPrefix
    }



-- DEFAULT CONFIG


type alias DefaultConfig =
    { bucket : Maybe String
    , publicUrlPrefix : Maybe String
    , folderPrefix : Maybe String
    }


decodeDefaultConfig : JSON.Decoder DefaultConfig
decodeDefaultConfig =
    let
        field =
            \name -> JSON.field name JSON.string

        optField =
            \name -> JSON.maybe (field name)
    in
    JSON.map3 DefaultConfig
        (optField "bucket")
        (optField "publicUrlPrefix")
        (optField "folderPrefix")


getDefaultConfig : Cmd Msg
getDefaultConfig =
    Http.get
        { url = "defaults.json"
        , expect = Http.expectJson ReceiveDefaultConfig decodeDefaultConfig
        }



-- VIEW


viewForm : Model -> Html Msg
viewForm form =
    let
        fullWidth =
            Html.Attributes.style "width" "100%"
    in
    dialog
        { dialogConfig
            | open = True
            , onClose = Nothing
        }
        { title = Nothing
        , content =
            [ textField
                { textFieldConfig
                    | label = Just "Bucket Name"
                    , onInput = Just UpdateBucket
                    , required = True
                    , value = form.bucket
                    , additionalAttributes = [ fullWidth ]
                }
            , Html.br [] []
            , textField
                { textFieldConfig
                    | label = Just "Access Key"
                    , onInput = Just UpdateAccessKey
                    , required = True
                    , additionalAttributes = [ fullWidth ]
                }
            , Html.br [] []
            , textField
                { textFieldConfig
                    | label = Just "Secret Key"
                    , onInput = Just UpdateSecretKey
                    , required = True
                    , type_ = "password"
                    , additionalAttributes = [ fullWidth ]
                }
            , Html.br [] []
            , textField
                { textFieldConfig
                    | label = Just "Folder Prefix"
                    , onInput = Just UpdateFolderPrefix
                    , required = False
                    , value = form.folderPrefix
                }
            , textField
                { textFieldConfig
                    | label = Just "Public URL Prefix"
                    , onInput = Just UpdatePublicUrlPrefix
                    , required = False
                    , value = form.publicUrlPrefix
                }
            ]
        , actions =
            [ textButton
                { buttonConfig
                    | onClick = Just SubmitForm
                }
                "Sign In"
            ]
        }
