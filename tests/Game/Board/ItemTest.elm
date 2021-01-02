module Game.Board.ItemTest exposing (suite)

import Expect
import Game.Board exposing (create)
import Game.Board.Item as Item
import Game.Direction exposing (Direction(..))
import Game.Domino as Domino exposing (Domino)
import Game.Domino.Play as Play exposing (Error(..), Play)
import Game.Domino.Suit as Suit exposing (Suit(..))
import Game.End as End exposing (End(..))
import Game.Event exposing (Event(..))
import Game.Fuzzer as Fuzzer
import Player as Player
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "Item"
        [ poseTest
        ]


poseTest : Test
poseTest =
    describe "pose"
        [ fuzz Fuzzer.suit "non-double" <|
            \x ->
                let
                    nonDoublePlay =
                        Domino.create x (Suit.different x) |> createPlay
                in
                nonDoublePlay
                    |> Item.pose { columns = 100, rows = 100 }
                    |> Expect.equal
                        (Item.create
                            { play = nonDoublePlay
                            , direction = West
                            , coordinates = { row = 50, column = 50 }
                            }
                        )
        , fuzz Fuzzer.suit "double" <|
            \x ->
                let
                    double =
                        Domino.create x x
                in
                createPlay double
                    |> Item.pose { columns = 100, rows = 100 }
                    |> Expect.equal
                        (Item.create
                            { play = double |> Domino.setDirection South |> createPlay
                            , direction = West
                            , coordinates = { row = 50, column = 50 }
                            }
                        )
        ]


createPlay : Domino -> Play
createPlay domino =
    Play.create (Player.opponent "a") domino End.Left



-- alignTest : Test
-- alignTest =
--     describe "align"
--         [ fuzz3 Fuzzer.suit Fuzzer.suit Fuzzer.suit "right - no align" <|
--             \x y z ->
--                 Play.align
--                     (Play.create { domino = Domino.create x y, playerName = Player.opponent "a", end = End.Left })
--                     (Play.create { domino = Domino.create y z, playerName = Player.opponent "b", end = End.Right })
--                     |> Expect.equal
--                         (Ok <| Play.create { domino = Domino.create y z, playerName = Player.opponent "b", end = End.Right })
--         , fuzz3 Fuzzer.suit Fuzzer.suit Fuzzer.suit "right - align" <|
--             \x y z ->
--                 Play.align
--                     (Play.create { domino = Domino.create x y, playerName = Player.opponent "a", end = End.Left })
--                     (Play.create { domino = Domino.create z y, playerName = Player.opponent "b", end = End.Right })
--                     |> Expect.equal
--                         ({ domino = Domino.create y z, playerName = Player.opponent "b", end = End.Right } |> Play.create |> Ok)
--         , fuzz Fuzzer.suit "right - error" <|
--             \x ->
--                 let
--                     y =
--                         Suit.different x
--                     z =
--                         Suit.different y
--                     pose =
--                         Play.create { domino = Domino.create x y, playerName = Player.opponent "a", end = End.Left }
--                     invalidPlay =
--                         Play.create { domino = Domino.create z z, playerName = Player.opponent "b", end = End.Right }
--                 in
--                 invalidPlay
--                     |> Play.align pose
--                     |> Expect.equal (Err <| Invalid { boardPlay = pose, play = invalidPlay })
--         , fuzz3 Fuzzer.suit Fuzzer.suit Fuzzer.suit "left - no align" <|
--             \x y z ->
--                 Play.align
--                     (Play.create { domino = Domino.create x y, playerName = Player.opponent "a", end = End.Left })
--                     (Play.create { domino = Domino.create z x, playerName = Player.opponent "b", end = End.Left })
--                     |> Expect.equal
--                         (Play.create { domino = Domino.create z x, playerName = Player.opponent "b", end = End.Left } |> Ok)
--         , fuzz3 Fuzzer.suit Fuzzer.suit Fuzzer.suit "left - align" <|
--             \x y z ->
--                 Play.align
--                     (Play.create { domino = Domino.create x y, playerName = Player.opponent "a", end = End.Left })
--                     (Play.create { domino = Domino.create x z, playerName = Player.opponent "b", end = End.Left })
--                     |> Expect.equal
--                         (Play.create { domino = Domino.create z x, playerName = Player.opponent "b", end = End.Left } |> Ok)
--         ]
