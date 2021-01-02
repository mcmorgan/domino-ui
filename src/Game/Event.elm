module Game.Event exposing
    ( Event(..)
    , decoder
    , getPass
    , getPlay
    , isForPlayerName
    )

import Game.Domino.Play as Play exposing (Play)
import Game.End exposing (End(..))
import Json.Decode exposing (Decoder, andThen, field, map, oneOf)
import Player as Player exposing (Name, Player)


type Event
    = Play Play
    | Pass (Player Name)


getPlay : Event -> Maybe Play
getPlay event =
    case event of
        Play p ->
            Just p

        _ ->
            Nothing


getPass : Event -> Maybe (Player Name)
getPass event =
    case event of
        Pass p ->
            Just p

        _ ->
            Nothing


isForPlayerName : Player Name -> Event -> Bool
isForPlayerName playerName event =
    case event of
        Pass playerName_ ->
            playerName == playerName_

        Play play ->
            play |> Play.isForPlayer playerName


decoder : Player Name -> Decoder Event
decoder me =
    oneOf
        [ field "player" (Player.decoder me)
            |> andThen Play.decoder
            |> map Play
        , field "player" (Player.decoder me)
            |> map Pass
        ]
