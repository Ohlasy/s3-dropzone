module SignIn exposing (Model, Msg, getDefaultConfig, init, update, viewForm)

import Html exposing (Html)
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
    dialog
        { dialogConfig
            | open = True
            , onClose = Nothing
        }
        { title = Nothing
        , content =
            [ textField
                { textFieldConfig
                    | placeholder = Just "Bucket Name"
                    , onInput = Just UpdateBucket
                    , fullwidth = True
                    , required = True
                    , value = Just form.bucket
                }
            , textField
                { textFieldConfig
                    | onInput = Just UpdateAccessKey
                    , fullwidth = True
                    , placeholder = Just "Access Key"
                    , required = True
                }
            , textField
                { textFieldConfig
                    | placeholder = Just "Secret Key"
                    , onInput = Just UpdateSecretKey
                    , fullwidth = True
                    , required = True
                    , type_ = "password"
                }
            , textField
                { textFieldConfig
                    | placeholder = Just "Folder Prefix"
                    , onInput = Just UpdateFolderPrefix
                    , fullwidth = True
                    , required = False
                    , value = Just form.folderPrefix
                }
            , textField
                { textFieldConfig
                    | placeholder = Just "Public URL Prefix"
                    , onInput = Just UpdatePublicUrlPrefix
                    , fullwidth = True
                    , required = False
                    , value = Just form.publicUrlPrefix
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
