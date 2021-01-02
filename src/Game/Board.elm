module Game.Board exposing
    ( Board
    , Error(..)
    , addPlay
    , create
    , errorToString
    , executeEvent
    , getEnd
    , select
    , setDimensions
    , view
    )

import Css exposing (auto, batch, margin, property)
import Game.Board.Item as Item exposing (Item)
import Game.Dimensions exposing (Dimensions)
import Game.Direction exposing (Direction(..))
import Game.Domino exposing (Domino)
import Game.Domino.Highlighter as Highlighter exposing (Highlighter)
import Game.Domino.Play as Play exposing (Play)
import Game.End exposing (End(..))
import Game.Event as Event exposing (Event)
import Game.Layout exposing (Layout(..))
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import NonEmptyList exposing (NonEmptyList)
import Player exposing (Msg)


type Board
    = Board ( State, Dimensions )


type State
    = Empty (Maybe Highlighter)
    | Active ( Maybe Highlighter, NonEmptyList Item, Maybe Highlighter )


type Error
    = AlreadyOnBoard Play
    | Play Play.Error
    | NoHighlighterFound


setDimensions : Dimensions -> Board -> Board
setDimensions dimensions (Board ( state, _ )) =
    Board ( state, dimensions )


getHighlighters : Board -> List Highlighter
getHighlighters (Board ( state, _ )) =
    case state of
        Empty highlighter ->
            List.filterMap identity [ highlighter ]

        Active ( left, _, right ) ->
            List.filterMap identity [ left, right ]


getItems : Board -> List Item
getItems (Board ( state, _ )) =
    case state of
        Empty _ ->
            []

        Active ( _, items, _ ) ->
            items |> NonEmptyList.toList


create : Dimensions -> List Play -> Result Error Board
create dimensions =
    List.foldl
        (Result.andThen << addPlay)
        (Board ( Empty Nothing, dimensions ) |> Ok)


addPlay : Play -> Board -> Result Error Board
addPlay play =
    addAlignedPlay
        (\(Board ( state, dimensions )) item ->
            case state of
                Empty _ ->
                    Board ( Active ( Nothing, NonEmptyList.singleton item, Nothing ), dimensions )

                Active ( _, items, _ ) ->
                    Board ( Active ( Nothing, items |> NonEmptyList.append item, Nothing ), dimensions )
        )
        play


executeEvent : Event -> Board -> Result Error Board
executeEvent event =
    case event of
        Event.Play play ->
            addPlay play

        Event.Pass _ ->
            Ok


select : List Play -> Board -> Result Error ( Board, NonEmptyList Highlighter )
select plays (Board ( state, dimensions )) =
    let
        clearedBoard =
            case state of
                Empty _ ->
                    Board ( Empty Nothing, dimensions )

                Active ( _, items, _ ) ->
                    Board ( Active ( Nothing, items, Nothing ), dimensions )

        addHighlighter (Board ( newState, _ )) item =
            let
                highlighter =
                    item |> Highlighter.create |> Just
            in
            case newState of
                Empty _ ->
                    Board ( Empty highlighter, dimensions )

                Active ( left, items, right ) ->
                    case item |> Item.getPlay |> Play.getEnd of
                        Left ->
                            Board ( Active ( highlighter, items, right ), dimensions )

                        Right ->
                            Board ( Active ( left, items, highlighter ), dimensions )
    in
    List.foldl (addAlignedPlay addHighlighter >> Result.andThen) (Ok clearedBoard) plays
        |> Result.andThen
            (\newBoard ->
                case newBoard |> getHighlighters |> NonEmptyList.fromList of
                    Ok highlighters ->
                        Ok ( newBoard, highlighters )

                    Err _ ->
                        Err NoHighlighterFound
            )


addAlignedPlay : (Board -> Item -> Board) -> Play -> Board -> Result Error Board
addAlignedPlay f play ((Board ( _, dimensions )) as board) =
    if board |> getItems |> List.map Item.getPlay |> List.any (Play.matches play) then
        Err <| AlreadyOnBoard play

    else
        case board |> getEnd (play |> Play.getEnd) of
            Just boardItem ->
                Play.align (boardItem |> Item.getPlay) play
                    |> Result.map (Item.align dimensions boardItem >> f board)
                    |> Result.mapError Play

            Nothing ->
                play |> Item.pose dimensions >> f board >> Ok


getEnd : End -> Board -> Maybe Item
getEnd end ((Board ( state, _ )) as board) =
    case state of
        Empty _ ->
            Nothing

        Active ( _, items, _ ) ->
            board
                |> getItems
                |> List.filter (Item.filterPlay (Play.getEnd >> (==) end))
                |> List.reverse
                |> List.head
                |> Maybe.withDefault
                    (items
                        |> NonEmptyList.head
                        |> Item.setEnd end
                    )
                |> Just


view : Board -> Html (Msg Domino)
view ((Board ( _, dimensions )) as board) =
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
        ((board |> getHighlighters |> List.map Highlighter.view)
            ++ (board |> getItems |> List.map (Item.view Play.view))
        )


errorToString : Error -> String
errorToString error =
    case error of
        AlreadyOnBoard play ->
            "Play - " ++ Play.toString play ++ " is already on the board"

        Play e ->
            e |> Play.errorToString

        NoHighlighterFound ->
            "NoHighlighterFound"
