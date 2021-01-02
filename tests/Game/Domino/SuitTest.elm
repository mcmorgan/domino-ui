module Game.Domino.SuitTest exposing (suite)

import Expect
import Game.Domino.Suit
    exposing
        ( Suit(..)
        , decoder
        , toInt
        )
import Json.Decode as Decode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Suit"
        [ decoderTest
        , toIntTest
        ]


decoderTest : Test
decoderTest =
    describe "decoder"
        [ test "Blank" <|
            \_ ->
                "0"
                    |> Decode.decodeString decoder
                    |> Expect.equal (Ok Blank)
        , test "Ace" <|
            \_ ->
                "1"
                    |> Decode.decodeString decoder
                    |> Expect.equal (Ok Ace)
        , test "Deuce" <|
            \_ ->
                "2"
                    |> Decode.decodeString decoder
                    |> Expect.equal (Ok Deuce)
        , test "Trey" <|
            \_ ->
                "3"
                    |> Decode.decodeString decoder
                    |> Expect.equal (Ok Trey)
        , test "Four" <|
            \_ ->
                "4"
                    |> Decode.decodeString decoder
                    |> Expect.equal (Ok Four)
        , test "Five" <|
            \_ ->
                "5"
                    |> Decode.decodeString decoder
                    |> Expect.equal (Ok Five)
        , test "Six" <|
            \_ ->
                "6"
                    |> Decode.decodeString decoder
                    |> Expect.equal (Ok Six)
        ]


toIntTest : Test
toIntTest =
    describe "toInt"
        [ test "Blank" <|
            \_ ->
                Blank
                    |> toInt
                    |> Expect.equal 0
        , test "Ace" <|
            \_ ->
                Ace
                    |> toInt
                    |> Expect.equal 1
        , test "Deuce" <|
            \_ ->
                Deuce
                    |> toInt
                    |> Expect.equal 2
        , test "Trey" <|
            \_ ->
                Trey
                    |> toInt
                    |> Expect.equal 3
        , test "Four" <|
            \_ ->
                Four
                    |> toInt
                    |> Expect.equal 4
        , test "Five" <|
            \_ ->
                Five
                    |> toInt
                    |> Expect.equal 5
        , test "Six" <|
            \_ ->
                Six
                    |> toInt
                    |> Expect.equal 6
        ]
