module ScoreTest exposing (suite)

import Expect
import Fuzz exposing (int)
import Json.Decode as Decode
import Score exposing (decoder)
import Test exposing (Test, describe, fuzz, fuzz2)


suite : Test
suite =
    describe "Score"
        [ decoderTest
        ]


decoderTest : Test
decoderTest =
    describe "decoder"
        [ fuzz int "individual" <|
            \i ->
                "{ \"score\": "
                    ++ String.fromInt i
                    ++ " }"
                    |> Decode.decodeString decoder
                    |> Expect.equal (Ok <| Score.individual i)
        , fuzz2 int int "team" <|
            \i t ->
                "{ \"score\": "
                    ++ String.fromInt i
                    ++ ", \"team_score\": "
                    ++ String.fromInt t
                    ++ " }"
                    |> Decode.decodeString decoder
                    |> Expect.equal (Ok <| Score.team i t)
        ]
