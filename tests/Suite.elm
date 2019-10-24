module Suite exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, string)
import Json.Decode
import Session exposing (Session, decodeSession, encodeSession, targetUrlForFile)
import Test exposing (..)


suite : Test
suite =
    describe "Session" <|
        [ fuzz3 string string string "Encoding roundtrip" <|
            \x y z ->
                let
                    session =
                        { accessKey = x
                        , secretKey = y
                        , bucket = z
                        , region = x
                        , publicUrlPrefix = y
                        , folderPrefix = z
                        }
                in
                session
                    |> encodeSession
                    |> Json.Decode.decodeValue decodeSession
                    |> Expect.equal (Ok session)
        ]
            ++ List.map testUrlPattern urlConstructionPatterns


type alias UrlConstructionPattern =
    { region : String
    , bucket : String
    , publicUrlPrefix : String
    , folderPrefix : String
    , fileName : String
    , resultUrl : String
    }


urlConstructionPatterns : List UrlConstructionPattern
urlConstructionPatterns =
    [ UrlConstructionPattern "eu-central-1" "test" "" "" "foo" "https://test.s3.eu-central-1.amazonaws.com/foo"
    , UrlConstructionPattern "eu-central-1" "test" "" "dir" "foo" "https://test.s3.eu-central-1.amazonaws.com/dir/foo"
    , UrlConstructionPattern "eu-central-1" "test" "" "/dir/" "foo" "https://test.s3.eu-central-1.amazonaws.com/dir/foo"
    , UrlConstructionPattern "eu-central-1" "test" "" "/" "foo" "https://test.s3.eu-central-1.amazonaws.com/foo"
    , UrlConstructionPattern "eu-central-1" "test" "https://foo.info/" "/dir/" "foo" "https://foo.info/dir/foo"
    ]


testUrlPattern : UrlConstructionPattern -> Test
testUrlPattern pat =
    test (Debug.toString pat) <|
        \_ ->
            let
                empty =
                    Session.init

                session =
                    { empty
                        | publicUrlPrefix = pat.publicUrlPrefix
                        , folderPrefix = pat.folderPrefix
                        , bucket = pat.bucket
                        , region = pat.region
                    }
            in
            targetUrlForFile pat.fileName session
                |> Expect.equal pat.resultUrl
