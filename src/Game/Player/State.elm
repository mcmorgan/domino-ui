module Game.Player.State exposing
    ( State
    , decoder
    , getPlays
    , isTurn
    , messageView
    , pass
    , toString
    , turn
    , waiting
    , winner
    )

import Game.Domino.Play as Play exposing (Play)
import Html.Styled exposing (Html, text)
import Json.Decode as Decode exposing (Decoder, andThen, bool, fail, field, list, maybe, oneOf, succeed)
import Player exposing (Name, Player(..))


type State
    = Waiting
    | Turn (List Play)
    | Pass
    | Winner


waiting : State
waiting =
    Waiting


turn : List Play -> State
turn =
    Turn


winner : State
winner =
    Winner


pass : State
pass =
    Pass


isTurn : State -> Bool
isTurn s =
    case s of
        Turn _ ->
            True

        Pass ->
            True

        _ ->
            False


decoder : Player Name -> Maybe (Player Name) -> Decoder State
decoder me winningPlayer =
    oneOf
        [ winnerDecoder me winningPlayer
        , turnDecoder me
        , passDecoder
        , waitingDecoder
        ]


winnerDecoder : Player Name -> Maybe (Player Name) -> Decoder State
winnerDecoder me w =
    field "id" (Player.decoder me)
        |> andThen
            (\p ->
                if w == Just p then
                    succeed Winner

                else
                    fail "Not winner, try another decoder..."
            )


turnDecoder : Player Name -> Decoder State
turnDecoder me =
    field "turn" bool
        |> andThen
            (\myTurn ->
                if myTurn then
                    maybe (field "playable" (list <| Play.decoder me))
                        |> andThen (\playable -> succeed <| turn (Maybe.withDefault [] playable))

                else
                    fail "Not my turn"
            )


passDecoder : Decoder State
passDecoder =
    Decode.field "pass" bool
        |> andThen
            (\isPlayerPass ->
                if isPlayerPass then
                    succeed pass

                else
                    fail "Not a pass"
            )


waitingDecoder : Decoder State
waitingDecoder =
    succeed Waiting


getPlays : State -> List Play
getPlays s =
    case s of
        Turn plays ->
            plays

        _ ->
            []


toString : State -> String
toString s =
    case s of
        Pass ->
            "pass"

        Turn _ ->
            "turn"

        Winner ->
            "winner"

        Waiting ->
            "waiting"


messageView : Player Name -> State -> Html msg
messageView player s =
    let
        playerName =
            Player.toName player
    in
    case s of
        Turn _ ->
            if player |> Player.isMe then
                playerName ++ ", it's your turn" |> text

            else
                playerName ++ "'s turn" |> text

        Pass ->
            playerName ++ "'s PASS" |> text

        Winner ->
            playerName ++ " wins the game" |> text

        Waiting ->
            text "..."
