port module Session exposing (..)

import Json.Decode exposing (Decoder, field, map2, string)
import Json.Encode as E


type alias Session =
    { accessKey : String
    , secretKey : String
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
        ]


decodeSession : Decoder Session
decodeSession =
    map2 Session
        (field "accessKey" string)
        (field "secretKey" string)
