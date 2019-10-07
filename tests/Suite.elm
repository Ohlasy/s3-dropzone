module Suite exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, string)
import Json.Decode
import Session exposing (Session, decodeSession, encodeSession)
import Test exposing (..)


suite : Test
suite =
    describe "Session"
        [ fuzz2 string string "Encoding roundtrip" <|
            \x y ->
                let
                    session =
                        { accessKey = x, secretKey = y }
                in
                session
                    |> encodeSession
                    |> Json.Decode.decodeValue decodeSession
                    |> Expect.equal (Ok session)
        ]
