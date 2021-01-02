module Game.DirectionTest exposing (suite)

import Expect
import Game.Direction exposing (Direction(..), next)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Position"
        [ nextTest
        ]


nextTest : Test
nextTest =
    describe "next"
        [ test "West" <| \_ -> West |> next |> Expect.equal South
        , test "South" <| \_ -> South |> next |> Expect.equal East
        , test "East" <| \_ -> East |> next |> Expect.equal North
        , test "North" <| \_ -> North |> next |> Expect.equal West
        ]
