module Game.Board.Item exposing
    ( Item
    , align
    , create
    , filterPlay
    , getId
    , getPlay
    , isEqual
    , pose
    , setEnd
    , view
    )

import Css exposing (batch, property)
import Game.Coordinates exposing (Coordinates)
import Game.Dimensions as Dimensions exposing (Dimensions)
import Game.Direction as Direction exposing (Direction(..))
import Game.Domino as Domino exposing (Domino)
import Game.Domino.Play as Play exposing (Play)
import Game.Domino.Suit exposing (Suit(..))
import Game.End exposing (End(..))
import Game.Layout exposing (Layout(..))
import Game.Orientation exposing (Orientation(..))
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Player exposing (Msg)


type Item
    = Item Model


type alias Model =
    { coordinates : Coordinates
    , play : Play
    , direction : Direction
    }


create : Model -> Item
create =
    Item


isEqual : Item -> Item -> Bool
isEqual (Item one) (Item other) =
    Play.matches one.play other.play


getId : Item -> String
getId (Item { play }) =
    [ "item", Play.getId play ] |> String.join "-"


pose : Dimensions -> Play -> Item
pose dimensions play =
    let
        transform a =
            (round <| toFloat a / 2) + 1
    in
    create
        { play =
            play
                |> Play.setDirection East
                |> Play.turnIfDouble
        , coordinates =
            { row = transform dimensions.rows
            , column = transform dimensions.columns
            }
        , direction = West
        }


align : Dimensions -> Item -> Play -> Item
align dimensions ((Item { direction }) as item) play =
    case item |> alignDirection direction play of
        alignedItem ->
            if alignedItem |> fallsOffBoard dimensions then
                item |> alignDirection (Direction.turn direction) play

            else
                alignedItem


alignDirection : Direction -> Play -> Item -> Item
alignDirection newDirection newPlay (Item { coordinates, play, direction }) =
    let
        alignedPlay =
            newPlay
                |> Play.setDirection
                    (case play |> Play.getEnd of
                        Left ->
                            newDirection |> Direction.opposite

                        Right ->
                            newDirection
                    )
                |> Play.turnIfDouble

        alignedPlayOffset =
            alignedPlay |> Play.getDimensions |> Dimensions.half

        offset =
            Dimensions.add
                alignedPlayOffset
                (play |> Play.getDimensions |> Dimensions.half)

        turnOffset =
            if
                (newDirection /= direction)
                    && ((alignedPlay |> Play.getDomino |> Domino.isDouble)
                            || ((alignedPlay |> Play.getDomino |> Domino.isDouble |> not)
                                    && (play |> Play.getDomino |> Domino.isDouble |> not)
                               )
                       )
            then
                alignedPlayOffset

            else
                Dimensions.empty

        newCoordinates =
            case newDirection of
                North ->
                    { coordinates
                        | row = coordinates.row - offset.rows
                        , column = coordinates.column - turnOffset.columns
                    }

                South ->
                    { coordinates
                        | row = coordinates.row + offset.rows
                        , column = coordinates.column + turnOffset.columns
                    }

                East ->
                    { coordinates
                        | column = coordinates.column + offset.columns
                        , row = coordinates.row - turnOffset.rows
                    }

                West ->
                    { coordinates
                        | column = coordinates.column - offset.columns
                        , row = coordinates.row + turnOffset.rows
                    }
    in
    Item
        { play = alignedPlay
        , coordinates = newCoordinates
        , direction = newDirection
        }


fallsOffBoard : Dimensions -> Item -> Bool
fallsOffBoard { rows, columns } ((Item { direction }) as item) =
    let
        { topLeft, bottomRight } =
            getBounding item

        minimumFeeSpace =
            Domino.short
    in
    case direction of
        North ->
            topLeft.row < minimumFeeSpace

        South ->
            bottomRight.row > rows - minimumFeeSpace

        East ->
            bottomRight.column > columns - minimumFeeSpace

        West ->
            topLeft.column < minimumFeeSpace


filterPlay : (Play -> Bool) -> Item -> Bool
filterPlay f (Item { play }) =
    f play


setEnd : End -> Item -> Item
setEnd end (Item ({ play } as model)) =
    let
        newDirection =
            case end of
                Left ->
                    East

                Right ->
                    East
    in
    Item { model | direction = newDirection, play = play |> Play.setEnd end }


getPlay : Item -> Play
getPlay (Item { play }) =
    play


getBounding : Item -> { topLeft : Coordinates, bottomRight : Coordinates }
getBounding (Item { play, coordinates }) =
    let
        ( offsetWidth, offsetHeight ) =
            case Play.getDimensions play of
                { columns, rows } ->
                    ( round (toFloat columns / 2), round (toFloat rows / 2) )
    in
    { topLeft =
        { row = coordinates.row - offsetHeight
        , column = coordinates.column - offsetWidth
        }
    , bottomRight =
        { row = coordinates.row + offsetHeight
        , column = coordinates.column + offsetWidth
        }
    }


view : (Play -> Html (Msg Domino)) -> Item -> Html (Msg Domino)
view f ((Item { play }) as item) =
    let
        toGridRange ( x, y ) =
            [ x, y ] |> List.map String.fromInt |> String.join "/"

        { topLeft, bottomRight } =
            getBounding item
    in
    div
        [ css
            [ batch
                [ property "grid-row" <|
                    toGridRange ( topLeft.row, bottomRight.row )
                , property "grid-column" <|
                    toGridRange ( topLeft.column, bottomRight.column )
                ]
            ]
        ]
        [ f play ]
