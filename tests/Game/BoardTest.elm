module Game.BoardTest exposing (suite)

import Expect
import Game.Board as Board
    exposing
        ( Board
        , Error(..)
        , create
        , getEnd
        )
import Game.Board.Item as Item exposing (Item)
import Game.Direction exposing (Direction(..))
import Game.Domino as Domino
import Game.Domino.Play as Play
import Game.Domino.Suit as Suit exposing (Suit(..))
import Game.End as End exposing (End(..))
import Game.Event exposing (Event(..))
import Game.Fuzzer as Fuzzer
import Player as Player
import Test exposing (Test, describe, fuzz, fuzz2, test)


suite : Test
suite =
    describe "Board"
        [ createTest
        , getEndTest
        ]


createTest : Test
createTest =
    describe "createTest"
        [ fuzz2 Fuzzer.suit Fuzzer.suit "prevents duplicates" <|
            \l r ->
                let
                    play =
                        Play.create
                            (Player.opponent "a")
                            (Domino.create l r)
                            End.Left
                in
                [ play
                , play
                ]
                    |> create { columns = 10, rows = 10 }
                    |> Expect.equal (Err <| AlreadyOnBoard play)
        , fuzz Fuzzer.suit "does not match end" <|
            \a ->
                let
                    b =
                        Suit.different a

                    c =
                        Suit.different b

                    d =
                        Suit.different c

                    pose =
                        Play.create
                            (Player.opponent "a")
                            (Domino.create a b)
                            End.Left

                    invalidPlay =
                        Play.create
                            (Player.opponent "b")
                            (Domino.create c d)
                            End.Left
                in
                [ pose
                , invalidPlay
                ]
                    |> create { columns = 10, rows = 10 }
                    |> Expect.equal (Err <| Board.Play <| Play.Invalid (Domino.create c d) End.Left a)
        ]


getEndTest : Test
getEndTest =
    let
        createPose l r =
            Play.create
                (Player.opponent "a")
                (Domino.create l r)
                End.Left
    in
    describe "getEnd"
        [ test "empty board" <|
            \_ ->
                []
                    |> create { columns = 10, rows = 10 }
                    |> Result.map (getEnds >> Expect.equal { left = Nothing, right = Nothing })
                    |> Result.mapError (Board.errorToString >> Expect.fail)
                    |> Result.withDefault (Expect.fail "should not get here")
        , fuzz Fuzzer.suit "pose only" <|
            \l ->
                let
                    pose =
                        createPose l (Suit.different l)
                in
                [ pose
                ]
                    |> create { columns = 10, rows = 10 }
                    |> Result.map
                        (getEnds
                            >> Expect.equal
                                { left =
                                    Just <|
                                        Item.create
                                            { play = pose
                                            , direction = West
                                            , coordinates = { row = 6, column = 6 }
                                            }
                                , right =
                                    Just <|
                                        Item.create
                                            { play = pose |> Play.setEnd End.Right
                                            , direction = East
                                            , coordinates = { row = 6, column = 6 }
                                            }
                                }
                        )
                    |> Result.mapError (Board.errorToString >> Expect.fail)
                    |> Result.withDefault (Expect.fail "should not get here")
        , test "with multiple dominoes on the board" <|
            \_ ->
                [ Play.create
                    (Player.opponent "a")
                    (Domino.create Six Six)
                    End.Left
                , Play.create
                    (Player.opponent "b")
                    (Domino.create Six Deuce)
                    End.Right
                , Play.create
                    (Player.opponent "c")
                    (Domino.create Deuce Four)
                    End.Right
                , Play.create
                    (Player.opponent "d")
                    (Domino.create Five Six)
                    End.Left
                ]
                    |> create { columns = 10, rows = 10 }
                    |> Result.map
                        (getEnds
                            >> Expect.equal
                                { left =
                                    Just <|
                                        Item.create
                                            { play =
                                                Play.create
                                                    (Player.opponent "d")
                                                    (Domino.create Five Six |> Domino.setDirection South)
                                                    End.Left
                                            , direction = North
                                            , coordinates = { row = 2, column = 6 }
                                            }
                                , right =
                                    Just <|
                                        Item.create
                                            { play =
                                                Play.create
                                                    (Player.opponent "c")
                                                    (Domino.create Deuce Four |> Domino.setDirection West)
                                                    End.Right
                                            , direction = West
                                            , coordinates = { row = 11, column = 3 }
                                            }
                                }
                        )
                    |> Result.mapError (Board.errorToString >> Expect.fail)
                    |> Result.withDefault (Expect.fail "should not get here")
        ]


getEnds :
    Board
    ->
        { left : Maybe Item
        , right : Maybe Item
        }
getEnds board =
    { left = board |> Board.getEnd Left
    , right = board |> Board.getEnd Right
    }
