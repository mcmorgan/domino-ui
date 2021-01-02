module Game.Domino.Hidden exposing
    ( Hidden
    , create
    , view
    )

import Css
    exposing
        ( opacity
        , zero
        )
import Game.Direction as Direction exposing (Direction)
import Game.Domino.Separator as Separator
import Game.Domino.Suit as Suit exposing (Suit(..))
import Game.Orientation exposing (Orientation(..))
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import UI.Theme as Theme


type Hidden
    = Hidden Direction


create : Direction -> Hidden
create =
    Hidden


view : Hidden -> Html msg
view (Hidden direction) =
    div
        [ css
            (opacity zero
                :: Theme.domino.defaultStyles
                ++ Direction.styles direction
            )
        ]
        [ Blank |> Suit.view direction
        , Separator.blankView direction
        , Blank |> Suit.view direction
        ]
