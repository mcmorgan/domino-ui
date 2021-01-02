module Game.Domino.Unexposed exposing
    ( Unexposed
    , create
    , view
    )

import Css exposing (backgroundColor)
import Game.Direction as Direction exposing (Direction)
import Game.Domino.Separator as Separator
import Game.Domino.Suit as Suit exposing (Suit(..))
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import UI.Theme as Theme


type Unexposed
    = Unexposed Direction


create : Direction -> Unexposed
create =
    Unexposed


view : Unexposed -> Html msg
view (Unexposed direction) =
    div
        [ css
            (backgroundColor Theme.domino.bodyColor
                :: Theme.domino.defaultStyles
                ++ Direction.styles direction
            )
        ]
        [ Blank |> Suit.view direction
        , Separator.blankView direction
        , Blank |> Suit.view direction
        ]
