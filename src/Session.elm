port module Session exposing (..)

import Json.Decode exposing (Decoder, field, map6, string)
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


decodeSession : Decoder Session
decodeSession =
    map6 Session
        (field "accessKey" string)
        (field "secretKey" string)
        (field "bucket" string)
        (field "region" string)
        (field "publicUrlPrefix" string)
        (field "folderPrefix" string)
