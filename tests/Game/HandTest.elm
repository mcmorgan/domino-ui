module Game.HandTest exposing (suite)

import Expect
import Game.Direction exposing (Direction(..))
import Game.Domino as Domino
import Game.Domino.Play as Play
import Game.Domino.Suit as Suit exposing (Suit(..))
import Game.End exposing (End(..))
import Game.Fuzzer as Fuzzer
import Game.Hand
    exposing
        ( HandDomino(..)
        , addExposed
        , addPlayed
        , addUnexposed
        , create
        , decoder
        , expose
        , remove
        )
import Game.Player.State as State
import Json.Decode as Decode
import Player as Player
import Test exposing (Test, describe, fuzz2, test)


suite : Test
suite =
    describe "Domino"
        [ decoderTest
        , removeTest
        , exposeTest
        ]


decoderTest : Test
decoderTest =
    describe "decoder"
        [ test "decodes correctly" <|
            \_ ->
                "[ [1,2], [2,3], [4,5], [], [] ]"
                    |> Decode.decodeString (decoder South State.waiting [])
                    |> Expect.equal
                        (Ok
                            (List.foldl addExposed
                                (create South)
                                ([ Domino.create Ace Deuce
                                 , Domino.create Deuce Trey
                                 , Domino.create Four Five
                                 ]
                                    |> List.map (Domino.setDirection South)
                                )
                                |> addUnexposed
                                |> addUnexposed
                            )
                        )
        ]


removeTest : Test
removeTest =
    describe "remove"
        [ fuzz2 Fuzzer.suit Fuzzer.suit "remove exposed domino" <|
            \l r ->
                let
                    dominoToRemove =
                        Domino.create l r
                            |> Domino.setDirection South

                    otherDomino =
                        Domino.create (Suit.different l) (Suit.different r)
                            |> Domino.setDirection South

                    play =
                        Play.create (Player.opponent "x") dominoToRemove Left
                in
                (List.foldl addExposed (create South) [ dominoToRemove, otherDomino ] |> addUnexposed |> addUnexposed)
                    |> remove play
                    |> Expect.equal (create South |> addPlayed play |> addExposed otherDomino |> addUnexposed |> addUnexposed)
        ]


exposeTest : Test
exposeTest =
    describe "exposeDomino"
        [ fuzz2 Fuzzer.suit Fuzzer.suit "exposes unexposed domino" <|
            \l r ->
                let
                    domino =
                        Domino.create l r

                    play =
                        Play.create (Player.opponent "x") domino Left
                in
                (create South |> addUnexposed |> addUnexposed |> addUnexposed |> addUnexposed)
                    |> expose play
                    |> Expect.equal (create South |> addUnexposed |> addUnexposed |> addUnexposed |> addExposed domino)
        , fuzz2 Fuzzer.suit Fuzzer.suit "domino already exposed" <|
            \l r ->
                let
                    domino =
                        Domino.create l r

                    hand =
                        create South |> addExposed domino |> addUnexposed |> addUnexposed |> addUnexposed
                in
                hand
                    |> expose (Play.create (Player.opponent "x") domino Left)
                    |> Expect.equal
                        hand
        ]
