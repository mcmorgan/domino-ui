module Game.Pack exposing (Pack, create, getDominoes)

import Game.Direction exposing (Direction(..))
import Game.Domino as Domino exposing (Domino)
import Game.Domino.Suit exposing (Suit(..))


type Pack
    = Pack (List Domino)


create : Direction -> Pack
create direction =
    [ ( Blank, Blank )
    , ( Blank, Ace )
    , ( Blank, Deuce )
    , ( Blank, Trey )
    , ( Blank, Four )
    , ( Blank, Five )
    , ( Blank, Six )
    , ( Ace, Ace )
    , ( Ace, Deuce )
    , ( Ace, Trey )
    , ( Ace, Four )
    , ( Ace, Five )
    , ( Ace, Six )
    , ( Deuce, Deuce )
    , ( Deuce, Trey )
    , ( Deuce, Four )
    , ( Deuce, Five )
    , ( Deuce, Six )
    , ( Trey, Trey )
    , ( Trey, Four )
    , ( Trey, Five )
    , ( Trey, Six )
    , ( Four, Four )
    , ( Four, Five )
    , ( Four, Six )
    , ( Five, Five )
    , ( Five, Six )
    , ( Six, Six )
    ]
        |> List.map
            (\( left, right ) ->
                Domino.create left right
                    |> Domino.setDirection direction
            )
        |> Pack


getDominoes : Pack -> List Domino
getDominoes (Pack dominoes) =
    dominoes
