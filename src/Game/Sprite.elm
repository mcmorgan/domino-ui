module Game.Sprite exposing (Sprite, create, view)

import Css exposing (displayFlex, flexWrap, wrap)
import Game.Board.Item as Item
import Game.Direction as Direction exposing (Direction(..))
import Game.Domino as Domino exposing (Domino)
import Game.Domino.Highlighter as Highlighter exposing (Highlighter)
import Game.Domino.Play as Play
import Game.Domino.Suit exposing (Suit(..))
import Game.Domino.Unexposed as Unexposed exposing (Unexposed)
import Game.End as End
import Game.Orientation exposing (Orientation(..))
import Game.Pack as Pack exposing (Pack)
import Html.Styled exposing (Html, div, h3, p, text)
import Html.Styled.Attributes exposing (css)
import Player exposing (Msg)


type Sprite
    = Sprite Direction Model


type alias Model =
    { pack : Pack
    , unexposed : Unexposed
    , highlighter : Highlighter
    }


create : Direction -> Sprite
create direction =
    { pack = Pack.create direction
    , unexposed = Unexposed.create direction
    , highlighter =
        Item.create
            { play =
                Play.create
                    (Player.opponent "mc")
                    (Domino.create Blank Blank |> Domino.setDirection direction)
                    End.Left
            , coordinates = { row = 5, column = 6 }
            , direction = East
            }
            |> Highlighter.create
    }
        |> Sprite direction


view : Sprite -> Html (Msg Domino)
view (Sprite direction { pack, unexposed, highlighter }) =
    div []
        [ h3 [] [ Direction.toString direction |> text ]
        , p [ css [ displayFlex, flexWrap wrap ] ]
            ((pack |> Pack.getDominoes |> List.map Domino.view)
                ++ [ unexposed |> Unexposed.view
                   , highlighter |> Highlighter.view
                   ]
            )
        ]
