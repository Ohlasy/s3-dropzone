module Suite exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, string)
import Json.Decode
import Session exposing (Session, decodeSession, encodeSession)
import Test exposing (..)


suite : Test
suite =
    describe "Session"
        [ fuzz3 string string string "Encoding roundtrip" <|
            \x y z ->
                let
                    session =
                        { accessKey = x, secretKey = y, bucket = z }
                in
                session
                    |> encodeSession
                    |> Json.Decode.decodeValue decodeSession
                    |> Expect.equal (Ok session)
        ]
