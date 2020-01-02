port module Session exposing (Session, decodeSession, deleteSession, encodeSession, init, saveSession, targetUrlForFile)

import Json.Decode as D
import Json.Encode as E


type alias Session =
    { accessKey : String
    , secretKey : String
    , bucket : String
    , region : String
    , publicUrlPrefix : String
    , folderPrefix : String
    }


port saveSession : E.Value -> Cmd msg


init : Session
init =
    { accessKey = ""
    , secretKey = ""
    , bucket = ""
    , region = ""
    , publicUrlPrefix = ""
    , folderPrefix = ""
    }



-- Persistence


deleteSession : Cmd msg
deleteSession =
    saveSession E.null


encodeSession : Session -> E.Value
encodeSession session =
    E.object
        [ ( "accessKey", E.string session.accessKey )
        , ( "secretKey", E.string session.secretKey )
        , ( "bucket", E.string session.bucket )
        , ( "region", E.string session.region )
        , ( "publicUrlPrefix", E.string session.publicUrlPrefix )
        , ( "folderPrefix", E.string session.folderPrefix )
        ]


decodeSession : D.Decoder Session
decodeSession =
    D.map6 Session
        (D.field "accessKey" D.string)
        (D.field "secretKey" D.string)
        (D.field "bucket" D.string)
        (D.field "region" D.string)
        (D.field "publicUrlPrefix" D.string)
        (D.field "folderPrefix" D.string)



-- Helpers


targetUrlForFile : String -> Session -> String
targetUrlForFile fileName session =
    let
        awsHost =
            "https://" ++ session.bucket ++ ".s3." ++ session.region ++ ".amazonaws.com"

        path =
            case session.folderPrefix of
                "" ->
                    "/" ++ fileName

                "/" ->
                    "/" ++ fileName

                _ ->
                    "/" ++ normalize session.folderPrefix ++ "/" ++ fileName

        host =
            case session.publicUrlPrefix of
                "" ->
                    awsHost

                customHost ->
                    stripTrailingSlash customHost
    in
    host ++ path


normalize : String -> String
normalize =
    stripLeadingSlash >> stripTrailingSlash


stripTrailingSlash : String -> String
stripTrailingSlash str =
    if String.endsWith "/" str then
        String.dropRight 1 str

    else
        str


stripLeadingSlash : String -> String
stripLeadingSlash str =
    if String.startsWith "/" str then
        String.dropLeft 1 str

    else
        str
