module Game.LayoutTest exposing (suite)

import Expect
import Game.Layout exposing (Layout(..), create)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Layout"
        [ createTest
        ]


createTest : Test
createTest =
    describe "create"
        [ test "landscape" <|
            \_ ->
                { scene =
                    { width = 1000
                    , height = 500
                    }
                , viewport =
                    { x = 0
                    , y = 0
                    , width = 1000
                    , height = 500
                    }
                }
                    |> create
                    |> Expect.equal Landscape
        , test "portrait" <|
            \_ ->
                { scene =
                    { width = 1000
                    , height = 1000
                    }
                , viewport =
                    { x = 0
                    , y = 0
                    , width = 1000
                    , height = 1000
                    }
                }
                    |> create
                    |> Expect.equal Square
        ]
