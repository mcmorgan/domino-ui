module Game.PlayTest exposing (suite)

import Expect
import Game.Board exposing (create)
import Game.Domino as Domino
import Game.Domino.Play as Play exposing (Error(..))
import Game.Domino.Suit as Suit exposing (Suit(..))
import Game.End as End exposing (End(..))
import Game.Event exposing (Event(..))
import Game.Fuzzer as Fuzzer
import Player as Player
import Test exposing (Test, describe, fuzz, fuzz3)


suite : Test
suite =
    describe "Play"
        [ alignTest
        ]


alignTest : Test
alignTest =
    describe "align"
        [ fuzz3 Fuzzer.suit Fuzzer.suit Fuzzer.suit "right - no align" <|
            \x y z ->
                Play.align
                    (Play.create (Player.opponent "a") (Domino.create x y) End.Left)
                    (Play.create (Player.opponent "b") (Domino.create y z) End.Right)
                    |> Expect.equal
                        (Ok <| Play.create (Player.opponent "b") (Domino.create y z) End.Right)
        , fuzz3 Fuzzer.suit Fuzzer.suit Fuzzer.suit "right - align" <|
            \x y z ->
                Play.align
                    (Play.create (Player.opponent "a") (Domino.create x y) End.Left)
                    (Play.create (Player.opponent "b") (Domino.create z y) End.Right)
                    |> Expect.equal (Play.create (Player.opponent "b") (Domino.create y z) End.Right |> Ok)
        , fuzz Fuzzer.suit "right - error" <|
            \x ->
                let
                    y =
                        Suit.different x

                    z =
                        Suit.different y

                    pose =
                        Play.create (Player.opponent "a") (Domino.create x y) End.Left

                    invalidPlay =
                        Play.create (Player.opponent "b") (Domino.create z z) End.Right
                in
                invalidPlay
                    |> Play.align pose
                    |> Expect.equal (Err <| Invalid (Domino.create z z) End.Right y)
        , fuzz3 Fuzzer.suit Fuzzer.suit Fuzzer.suit "left - no align" <|
            \x y z ->
                Play.align
                    (Play.create (Player.opponent "a") (Domino.create x y) End.Left)
                    (Play.create (Player.opponent "b") (Domino.create z x) End.Left)
                    |> Expect.equal
                        (Play.create (Player.opponent "b") (Domino.create z x) End.Left |> Ok)
        , fuzz3 Fuzzer.suit Fuzzer.suit Fuzzer.suit "left - align" <|
            \x y z ->
                Play.align
                    (Play.create (Player.opponent "a") (Domino.create x y) End.Left)
                    (Play.create (Player.opponent "b") (Domino.create x z) End.Left)
                    |> Expect.equal
                        (Play.create (Player.opponent "b") (Domino.create z x) End.Left |> Ok)
        ]
