module Game.Domino.Pip exposing
    ( Pip(..)
    , all
    , view
    )

import Css
    exposing
        ( batch
        , borderRadius
        , height
        , margin
        , pct
        , property
        , vmax
        , width
        )
import Html.Styled exposing (Html, span)
import Html.Styled.Attributes exposing (css)
import UI.Theme as Theme


type Pip
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine


all : List Pip
all =
    [ One
    , Two
    , Three
    , Four
    , Five
    , Six
    , Seven
    , Eight
    , Nine
    ]


view : Bool -> Pip -> Html msg
view on suit =
    let
        onStyle =
            if on then
                [ Css.batch
                    [ Css.property "background" "radial-gradient(closest-side, #444, #111)"
                    ]
                ]

            else
                []

        ( row, column ) =
            case suit of
                One ->
                    ( 1, 1 )

                Two ->
                    ( 1, 2 )

                Three ->
                    ( 1, 3 )

                Four ->
                    ( 2, 1 )

                Five ->
                    ( 2, 2 )

                Six ->
                    ( 2, 3 )

                Seven ->
                    ( 3, 1 )

                Eight ->
                    ( 3, 2 )

                Nine ->
                    ( 3, 3 )
    in
    span
        [ css
            ([ batch
                [ property "grid-row" (row |> String.fromInt)
                , property "grid-column" (column |> String.fromInt)
                ]
             , width (vmax (0.525 * Theme.domino.size))
             , height (vmax (0.525 * Theme.domino.size))
             , borderRadius (pct 50)
             , margin (vmax (0.1 * Theme.domino.size))
             ]
                ++ onStyle
            )
        ]
        []
