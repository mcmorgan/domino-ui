module Game.EventTest exposing (suite)

import Expect
import Game.Domino as Domino
import Game.Domino.Play as Play
import Game.Domino.Suit as Suit exposing (Suit(..))
import Game.End exposing (End(..))
import Game.Event
    exposing
        ( Event(..)
        , decoder
        )
import Game.Fuzzer as Fuzzer
import Json.Decode as Decode
import Player as Player
import Test exposing (Test, describe, fuzz2, test)


suite : Test
suite =
    describe "Domino"
        [ decoderTest
        ]


decoderTest : Test
decoderTest =
    describe "decoder"
        [ fuzz2 Fuzzer.suit Fuzzer.suit "Play with direction left" <|
            \l r ->
                ("{ \"player\": \"marcel\", \"direction\": \"left\", \"domino\": ["
                    ++ (String.join "," <| List.map (String.fromInt << Suit.toInt) [ l, r ])
                    ++ "] }"
                )
                    |> Decode.decodeString (decoder <| Player.current <| Just "marcel")
                    |> Expect.equal
                        (Play.create (Player.current <| Just "marcel") (Domino.create l r) Left |> Play |> Ok)
        , fuzz2 Fuzzer.suit Fuzzer.suit "Play with direction right" <|
            \l r ->
                ("{ \"player\": \"marcel\", \"direction\": \"right\", \"domino\": ["
                    ++ (String.join "," <| List.map (String.fromInt << Suit.toInt) [ l, r ])
                    ++ "] }"
                )
                    |> Decode.decodeString (decoder <| Player.current <| Just "marcel")
                    |> Expect.equal
                        (Play.create (Player.current <| Just "marcel") (Domino.create l r) Right |> Play |> Ok)
        , fuzz2 Fuzzer.suit Fuzzer.suit "Play without direction" <|
            \l r ->
                ("{ \"player\": \"marcel\", \"domino\": ["
                    ++ (String.join "," <| List.map (String.fromInt << Suit.toInt) [ l, r ])
                    ++ "] }"
                )
                    |> Decode.decodeString (decoder <| Player.current <| Just "marcel")
                    |> Expect.equal
                        (Play.create (Player.current <| Just "marcel") (Domino.create l r) Left |> Play |> Ok)
        , test "Pass play" <|
            \_ ->
                "{ \"player\": \"marcel\" }"
                    |> Decode.decodeString (decoder <| Player.current <| Just "marcel")
                    |> Expect.equal (Ok <| Pass <| Player.current <| Just "marcel")
        ]
