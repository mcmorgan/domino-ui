module Game.Domino.Highlighter exposing
    ( Highlighter
    , create
    , getId
    , getPlay
    , isEqual
    , view
    )

import Css
    exposing
        ( backgroundColor
        , hex
        , num
        , opacity
        )
import Game.Board.Item as Item exposing (Item)
import Game.Direction as Direction
import Game.Domino as Domino exposing (Domino)
import Game.Domino.Play as Play exposing (Play)
import Game.Domino.Separator as Separator
import Game.Domino.Suit as Suit exposing (Suit(..))
import Game.Orientation exposing (Orientation(..))
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css, id)
import Html.Styled.Events exposing (onClick)
import Player exposing (Msg)
import UI.Theme as Theme


type Highlighter
    = Highlighter Item


create : Item -> Highlighter
create =
    Highlighter


isEqual : Highlighter -> Highlighter -> Bool
isEqual (Highlighter one) (Highlighter other) =
    Item.isEqual one other


getId : Highlighter -> String
getId (Highlighter item) =
    [ "highligter", Item.getId item ] |> String.join "-"


getPlay : Highlighter -> Play
getPlay (Highlighter item) =
    item |> Item.getPlay


view : Highlighter -> Html (Msg Domino)
view ((Highlighter item) as highlighter) =
    let
        play =
            item |> Item.getPlay

        direction =
            play |> Play.getDomino |> Domino.getDirection
    in
    item
        |> Item.view
            (\_ ->
                div
                    [ id <| getId highlighter
                    , css
                        (Theme.domino.defaultStyles
                            ++ Direction.styles direction
                            ++ [ backgroundColor (hex "00ff00")
                               , opacity (num 0.75)
                               ]
                        )
                    , item |> Item.getPlay |> Play.toMsg |> onClick
                    ]
                    [ Blank |> Suit.view direction
                    , Separator.blankView direction
                    , Blank |> Suit.view direction
                    ]
            )
