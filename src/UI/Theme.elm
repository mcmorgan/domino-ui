module UI.Theme exposing (domino)

import Css
    exposing
        ( Color
        , Style
        , batch
        , border3
        , borderRadius
        , displayFlex
        , hex
        , margin
        , property
        , solid
        , vmax
        , zero
        )
import Game.Orientation exposing (Orientation(..))


{-| default theme
-}
domino :
    { bodyColor : Color
    , size : Float
    , defaultStyles : List Style
    }
domino =
    { bodyColor = hex "fffff0"
    , size = size
    , defaultStyles =
        [ border3 (vmax (0.1 * size)) solid (hex "444")
        , borderRadius (vmax (0.3 * size))
        , Css.padding3 (vmax -(0.1 * size)) zero (vmax -(0.1 * size))
        , margin (vmax (0.1 * size))
        , displayFlex
        , dropShadow
        , batch
            [ property "height" "min-content"
            , property "width" "min-content"
            ]
        ]
    }


dropShadow : Style
dropShadow =
    let
        dropShadowSize =
            String.fromFloat (size * 0.1) ++ "vmax"
    in
    batch
        [ property "filter" <|
            "drop-shadow("
                ++ dropShadowSize
                ++ " "
                ++ dropShadowSize
                ++ " "
                ++ dropShadowSize
                ++ " #333)"
        ]


size : Float
size =
    1.25
