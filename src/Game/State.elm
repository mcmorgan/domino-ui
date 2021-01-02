module Game.State exposing
    ( State(..)
    , decoder
    , isActive
    , isCompleted
    , isSetCompleted
    , view
    )

import Game.Player as GamePlayer exposing (GamePlayer)
import Html.Styled exposing (Html, text)
import Json.Decode exposing (Decoder, andThen, fail, field, maybe, string, succeed)
import Player as Player exposing (Name, Player(..))


type Win
    = WinOutright
    | WinKey


type State
    = Active
    | Drawn
    | Completed Win (Player Name)
    | SetCompleted Win (Player Name)


isActive : State -> Bool
isActive s =
    case s of
        Active ->
            True

        _ ->
            False


isCompleted : State -> Bool
isCompleted s =
    case s of
        Completed _ _ ->
            True

        Drawn ->
            True

        _ ->
            False


isSetCompleted : State -> Bool
isSetCompleted s =
    case s of
        SetCompleted _ _ ->
            True

        _ ->
            False


decoder : Maybe (Player Name) -> Decoder State
decoder winner =
    andThen
        (\outcome ->
            andThen
                (\state ->
                    case ( state, outcome, winner ) of
                        ( "active", Nothing, Nothing ) ->
                            Active |> succeed

                        ( "completed", Just "drawn", Nothing ) ->
                            Drawn |> succeed

                        ( "completed", Just "win_outright", Just player ) ->
                            Completed WinOutright player |> succeed

                        ( "completed", Just "key", Just player ) ->
                            Completed WinKey player |> succeed

                        ( "set_completed", Just "win_outright", Just player ) ->
                            SetCompleted WinOutright player |> succeed

                        ( "set_completed", Just "key", Just player ) ->
                            SetCompleted WinKey player |> succeed

                        _ ->
                            "Unknown game state: "
                                ++ state
                                ++ ", outcome: "
                                ++ Maybe.withDefault "" outcome
                                ++ ", winner: "
                                ++ Maybe.withDefault "" (Maybe.map Player.toName winner)
                                |> fail
                )
                (field "state" string)
        )
        (maybe <| field "outcome" string)


view : List GamePlayer -> State -> Html msg
view players s =
    case s of
        Active ->
            case players |> List.filter GamePlayer.isTurn |> List.head of
                Just player ->
                    player |> GamePlayer.statusView

                Nothing ->
                    "Sending play, please wait..." |> text

        Completed WinKey winner ->
            Player.toName winner ++ " wins the game via key (+2)" |> text

        Completed WinOutright winner ->
            Player.toName winner ++ " wins the game" |> text

        Drawn ->
            "The game is drawn! Double six to pose for the next game." |> text

        SetCompleted WinKey winner ->
            Player.toName winner ++ " wins the set via key (+2)!" |> text

        SetCompleted WinOutright winner ->
            Player.toName winner ++ " wins the set!" |> text
