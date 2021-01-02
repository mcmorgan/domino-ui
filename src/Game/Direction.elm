module Game.Direction exposing
    ( Direction(..)
    , all
    , next
    , opposite
    , styles
    , toOrientation
    , toString
    , turn
    )

import Css exposing (Style)
import Game.Orientation as Orientation exposing (Orientation(..))


type Direction
    = North
    | South
    | East
    | West


turn : Direction -> Direction
turn d =
    case d of
        North ->
            East

        South ->
            West

        West ->
            North

        East ->
            South


opposite : Direction -> Direction
opposite d =
    case d of
        North ->
            South

        South ->
            North

        West ->
            East

        East ->
            West


all : List Direction
all =
    [ North
    , South
    , East
    , West
    ]


toOrientation : Direction -> Orientation
toOrientation d =
    case d of
        North ->
            Vertical

        South ->
            Vertical

        West ->
            Horizontal

        East ->
            Horizontal


styles : Direction -> List Style
styles direction =
    direction |> toOrientation |> Orientation.styles


toString : Direction -> String
toString d =
    case d of
        North ->
            "North"

        South ->
            "South"

        West ->
            "West"

        East ->
            "East"


next : Direction -> Direction
next p =
    case p of
        West ->
            South

        South ->
            East

        East ->
            North

        North ->
            West
