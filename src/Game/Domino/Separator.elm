module Game.Domino.Separator exposing (blankView, view)

import Css
    exposing
        ( Style
        , alignSelf
        , backgroundColor
        , height
        , hex
        , margin2
        , middle
        , vmax
        , width
        )
import Game.Direction as Direction exposing (Direction(..))
import Game.Orientation exposing (Orientation(..))
import Html.Styled exposing (Html, span)
import Html.Styled.Attributes exposing (css)
import UI.Theme as Theme


view : Direction -> Html msg
view direction =
    span
        [ css
            ([ backgroundColor (hex "333")
             , alignSelf middle
             ]
                ++ directionStyles direction
            )
        ]
        []


blankView : Direction -> Html msg
blankView direction =
    span [ css (alignSelf middle :: directionStyles direction) ] []


directionStyles : Direction -> List Style
directionStyles direction =
    case direction |> Direction.toOrientation of
        Horizontal ->
            [ width (vmax (0.25 * Theme.domino.size))
            , margin2
                (vmax (0.13 * Theme.domino.size))
                (vmax (-0.025 * Theme.domino.size))
            ]

        Vertical ->
            [ height (vmax (0.25 * Theme.domino.size))
            , margin2
                (vmax (-0.025 * Theme.domino.size))
                (vmax (0.13 * Theme.domino.size))
            ]
