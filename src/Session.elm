port module Session exposing (..)

import Json.Decode exposing (Decoder, field, map3, string)
import Json.Encode as E


type alias Session =
    { accessKey : String
    , secretKey : String
    , bucket : String
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
        ]


decodeSession : Decoder Session
decodeSession =
    map3 Session
        (field "accessKey" string)
        (field "secretKey" string)
        (field "bucket" string)
