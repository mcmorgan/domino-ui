module Game.Orientation exposing
    ( Orientation(..)
    , opposite
    , styles
    , toString
    )

import Css
    exposing
        ( Style
        , column
        , flexDirection
        , row
        )


type Orientation
    = Vertical
    | Horizontal


styles : Orientation -> List Style
styles orientation =
    case orientation of
        Horizontal ->
            [ flexDirection row ]

        Vertical ->
            [ flexDirection column ]


opposite : Orientation -> Orientation
opposite orientation =
    case orientation of
        Vertical ->
            Horizontal

        Horizontal ->
            Vertical


toString : Orientation -> String
toString orientation =
    case orientation of
        Horizontal ->
            "Horizontal"

        Vertical ->
            "Vertical"
