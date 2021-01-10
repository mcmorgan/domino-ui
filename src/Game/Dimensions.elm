module Game.Dimensions exposing
    ( Dimensions
    , add
    , empty
    , grid
    , half
    )

import Css exposing (auto, batch, margin, property)
import Html.Styled exposing (Html, div)
import Player exposing (Msg)
import Svg.Styled.Attributes exposing (css)


type alias Dimensions =
    { columns : Int
    , rows : Int
    }


add : Dimensions -> Dimensions -> Dimensions
add { columns, rows } other =
    { columns = columns + other.columns
    , rows = rows + other.rows
    }


half : Dimensions -> Dimensions
half { columns, rows } =
    let
        calculate dimension =
            toFloat dimension / 2 |> round
    in
    { columns = calculate columns
    , rows = calculate rows
    }


empty : Dimensions
empty =
    { rows = 0, columns = 0 }


grid : List (Html (Msg a)) -> Dimensions -> Html (Msg a)
grid elements dimensions =
    div
        [ css
            [ margin auto
            , batch
                [ property "display" "grid"
                , property "grid-template-columns" <|
                    "repeat("
                        ++ String.fromInt dimensions.columns
                        ++ ", 1.6vmax)"
                , property "grid-template-rows" <|
                    "repeat("
                        ++ String.fromInt dimensions.rows
                        ++ ", 1.6vmax)"
                , property "gap" "0.1vmax"
                ]
            ]
        ]
        elements
