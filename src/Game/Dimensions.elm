module Game.Dimensions exposing
    ( Dimensions
    , add
    , empty
    , half
    )


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
