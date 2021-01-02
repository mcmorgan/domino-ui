module Game.Player.StateTest exposing (suite)

import Expect
import Game.Domino as Domino
import Game.Domino.Play as Play
import Game.Domino.Suit exposing (Suit(..))
import Game.End as End exposing (End(..))
import Game.Player.State
    exposing
        ( decoder
        , pass
        , turn
        , waiting
        , winner
        )
import Json.Decode as Decode
import Player as Player exposing (Name, Player)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Domino"
        [ decoderTest
        ]


decoderTest : Test
decoderTest =
    describe "decoder"
        [ test "my turn" <|
            \_ ->
                """
                {
                    "id": "me",
                    "turn": true,
                    "playable": [
                        {
                            "domino": [1, 2],
                            "direction": "left"
                        }
                    ]
                }
                """
                    |> Decode.decodeString (decoder me Nothing)
                    |> Expect.equal
                        (Ok <| turn [ Play.create me (Domino.create Ace Deuce) End.Left ])
        , test "opponent's turn" <|
            \_ ->
                """
                {
                    "id": "not_me",
                    "turn": true
                }
                """
                    |> Decode.decodeString (decoder me Nothing)
                    |> Expect.equal (Ok <| turn [])
        , test "pass" <|
            \_ ->
                """
                {
                    "pass": true
                }
                """
                    |> Decode.decodeString (decoder me Nothing)
                    |> Expect.equal (Ok <| pass)
        , test "winner is me" <|
            \_ ->
                """
                {
                    "id": "me"
                }
                """
                    |> Decode.decodeString (decoder me (Just <| me))
                    |> Expect.equal (Ok <| winner)
        , test "winner is not me" <|
            \_ ->
                """
                {
                    "id": "me"
                }
                """
                    |> Decode.decodeString (decoder me (Just <| Player.opponent "not-me"))
                    |> Expect.equal (Ok <| waiting)
        ]


me : Player Name
me =
    Player.me "me"
