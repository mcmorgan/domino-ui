module Game.DominoTest exposing (suite)

import Expect
import Game.Direction exposing (Direction(..))
import Game.Domino
    exposing
        ( create
        , decoder
        , flip
        , getDimensions
        , getSuit
        , isDouble
        , matches
        , setDirection
        )
import Game.Domino.Suit as Suit exposing (Suit(..), toInt)
import Game.End as End exposing (End(..))
import Game.Fuzzer as Fuzzer
import Json.Decode as Decode
import Test exposing (Test, describe, fuzz, fuzz2)


suite : Test
suite =
    describe "Domino"
        [ matchesTest
        , flipTest
        , getSuitTest
        , isDoubleTest
        , decoderTest
        , getDimensionsTest
        ]


decoderTest : Test
decoderTest =
    describe "decoder"
        [ fuzz2 Fuzzer.suit Fuzzer.suit "Exposed domino" <|
            \l r ->
                ("[" ++ (String.join "," <| List.map (String.fromInt << Suit.toInt) [ l, r ]) ++ "]")
                    |> Decode.decodeString decoder
                    |> Expect.equal (Ok (create l r))
        ]


matchesTest : Test
matchesTest =
    describe "matches"
        [ fuzz2 Fuzzer.suit Fuzzer.suit "Same left and right" <|
            \l r ->
                create l r
                    |> matches (create l r)
                    |> Expect.true "Dominoes equal"
        , fuzz2 Fuzzer.suit Fuzzer.suit "Swapped left and right" <|
            \l r ->
                create l r
                    |> matches (create r l)
                    |> Expect.true "Dominoes equal"
        , fuzz2 Fuzzer.suit Fuzzer.suit "Different dominoes" <|
            \l r ->
                create (Suit.different l) r
                    |> matches (create l r)
                    |> Expect.false "Dominoes different"
        ]


flipTest : Test
flipTest =
    describe "flip"
        [ fuzz2 Fuzzer.suit Fuzzer.suit "create" <|
            \l r ->
                create l r
                    |> flip
                    |> Expect.equal (create r l)
        ]


getDimensionsTest : Test
getDimensionsTest =
    describe "getDimensions"
        [ fuzz2 Fuzzer.suit Fuzzer.suit "Horizontal" <|
            \l r ->
                create l r
                    |> setDirection East
                    |> getDimensions
                    |> Expect.equal { columns = 4, rows = 2 }
        , fuzz2 Fuzzer.suit Fuzzer.suit "Vertical" <|
            \l r ->
                create l r
                    |> setDirection North
                    |> getDimensions
                    |> Expect.equal { columns = 2, rows = 4 }
        ]


getSuitTest : Test
getSuitTest =
    describe "getEnd"
        [ fuzz2 Fuzzer.suit Fuzzer.suit "Left" <|
            \l r ->
                getSuit (create l r) End.Left
                    |> Expect.equal l
        , fuzz2 Fuzzer.suit Fuzzer.suit "Right" <|
            \l r ->
                getSuit (create l r) End.Right
                    |> Expect.equal r
        ]


isDoubleTest : Test
isDoubleTest =
    describe "double"
        [ fuzz Fuzzer.suit "is double" <|
            \x ->
                create x x
                    |> isDouble
                    |> Expect.true "was not double"
        , fuzz Fuzzer.suit "non-double" <|
            \x ->
                create x (Suit.different x)
                    |> isDouble
                    |> Expect.false "but was double"
        ]
