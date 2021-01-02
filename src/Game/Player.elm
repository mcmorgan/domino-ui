module Game.Player exposing
    ( GamePlayer
    , Type
    , decoder
    , executeEvent
    , getPlayable
    , getPlayerName
    , getPresence
    , isMe
    , isTurn
    , moveDominoBack
    , moveDominoBy
    , select
    , setWaiting
    , stageEvent
    , statusView
    , view
    )

import Game.Direction as Direction exposing (Direction(..))
import Game.Domino exposing (Domino)
import Game.Domino.Play as Play
import Game.Domino.Playable exposing (Playable)
import Game.Event as Event exposing (Event)
import Game.Hand as Hand exposing (Hand)
import Game.Player.Presence as Presence exposing (Presence)
import Game.Player.State as State exposing (State)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (classList)
import Json.Decode as Decode
    exposing
        ( Decoder
        , andThen
        , nullable
        , string
        , succeed
        )
import Json.Decode.Pipeline
    exposing
        ( custom
        , hardcoded
        , optional
        , required
        )
import Player exposing (Msg, Name, Player(..), view)
import Score as Score exposing (Score, decoder)


type GamePlayer
    = GamePlayer Model


type alias Model =
    { playerName : Player Name
    , score : Score
    , state : State
    , presence : Presence
    , type_ : Type
    , hand : Hand
    , action : Maybe Action
    }


type Type
    = Human
    | Bot


type Action
    = GivePartnerPose


select : Domino -> GamePlayer -> GamePlayer
select domino (GamePlayer m) =
    GamePlayer { m | hand = m.hand |> Hand.select domino }


moveDominoBy : Domino -> ( Float, Float ) -> GamePlayer -> GamePlayer
moveDominoBy domino delta (GamePlayer ({ hand } as model)) =
    GamePlayer { model | hand = hand |> Hand.moveDominoBy domino delta }


moveDominoBack : Domino -> GamePlayer -> GamePlayer
moveDominoBack domino (GamePlayer ({ hand } as model)) =
    GamePlayer { model | hand = hand |> Hand.moveDominoBack domino }


getPlayerName : GamePlayer -> Player Name
getPlayerName (GamePlayer m) =
    m.playerName


getPlayable : GamePlayer -> List Playable
getPlayable (GamePlayer { hand }) =
    hand |> Hand.getPlayable


getPresence : GamePlayer -> Presence
getPresence (GamePlayer m) =
    m.presence


isMe : GamePlayer -> Bool
isMe (GamePlayer m) =
    Player.isMe m.playerName


isTurn : GamePlayer -> Bool
isTurn (GamePlayer m) =
    m.state |> State.isTurn


setWaiting : GamePlayer -> GamePlayer
setWaiting (GamePlayer m) =
    GamePlayer { m | state = State.waiting }


statusView : GamePlayer -> Html msg
statusView (GamePlayer m) =
    m.state |> State.messageView m.playerName


executeEvent : Event -> GamePlayer -> GamePlayer
executeEvent event gamePlayer =
    let
        executePlay ((GamePlayer m) as p) play =
            if play |> Play.isForPlayer m.playerName then
                GamePlayer { m | hand = m.hand |> Hand.remove play |> Hand.unsetTurn }

            else
                p
    in
    event
        |> Event.getPlay
        |> Maybe.map (executePlay gamePlayer)
        |> Maybe.withDefault gamePlayer


stageEvent : Event -> GamePlayer -> GamePlayer
stageEvent event ((GamePlayer m) as gamePlayer) =
    let
        stagePlay play =
            if play |> Play.isForPlayer m.playerName then
                GamePlayer
                    { m
                        | hand = m.hand |> Hand.expose play
                        , state =
                            State.turn
                                (event
                                    |> Event.getPlay
                                    |> Maybe.map List.singleton
                                    |> Maybe.withDefault []
                                )
                    }

            else
                gamePlayer

        stagePass playerName =
            if playerName == m.playerName then
                GamePlayer { m | state = State.pass }

            else
                gamePlayer
    in
    event
        |> Event.getPass
        |> Maybe.map stagePass
        |> Maybe.withDefault
            (event
                |> Event.getPlay
                |> Maybe.map stagePlay
                |> Maybe.withDefault gamePlayer
            )



-- DECODERS


decoder : Player Name -> Maybe (Player Name) -> List Event -> Direction -> Decoder GamePlayer
decoder me winner events direction =
    succeed
        (\playerName state score ->
            Decode.succeed Model
                |> hardcoded playerName
                |> hardcoded score
                |> hardcoded state
                |> required "state" Presence.decoder
                |> required "type" typeDecoder
                |> required "dominoes" (Hand.decoder direction state (events |> List.filter (Event.isForPlayerName playerName)))
                |> optional "action" (nullable playerActionDecoder) Nothing
        )
        |> required "id" (Player.decoder me)
        |> custom (State.decoder me winner)
        |> custom Score.decoder
        |> andThen (Decode.map GamePlayer)


typeDecoder : Decoder Type
typeDecoder =
    string
        |> andThen
            (\s ->
                case s of
                    "human" ->
                        Decode.succeed Human

                    "bot" ->
                        Decode.succeed Bot

                    _ ->
                        Decode.fail ("Unknown player type: " ++ s)
            )


playerActionDecoder : Decoder Action
playerActionDecoder =
    string
        |> andThen
            (\s ->
                case s of
                    "give_partner_pose" ->
                        Decode.succeed <| GivePartnerPose

                    _ ->
                        Decode.fail <| "Invalid game action " ++ s
            )



-- VIEW


view : GamePlayer -> Html (Msg Domino)
view (GamePlayer { playerName, score, presence, state, hand }) =
    div
        [ classList
            [ ( "hand", True )
            , ( "hand__" ++ Direction.toString (hand |> Hand.getDirection), True )
            , ( "hand--" ++ State.toString state, True )
            ]
        ]
        [ hand |> Hand.view
        , div
            [ classList
                [ ( "player-score", True )
                , ( "player-score--offline", False )
                , ( "player-score--away", False )
                ]
            ]
            [ playerName |> Player.view presence
            , score |> Score.view
            ]
        ]
