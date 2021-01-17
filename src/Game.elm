module Game exposing
    ( Game
    , addChatMessage, clearChatDraft, closeChat, composeChat, decoder, errorToString, executeEvent, getChatDraft, getId, getTimesoutAt, hasChat, isResumable, isSetCompleted, isUpdating, moveSelectedBack, moveSelectedBy, nextRoundStartedTimeout, openChat, roundNextEvent, selectDomino, setConnectionStatus, setNextGame, setViewport, slug, stageEvent, switchToNextGame, switchToNextRoundGame, updateDecoder, view
    , create, view2
    )

{-| This makes is possible to represent a domino game using elm


# Definition

@docs Game


# Common Helpers

@docs addChatMessage, clearChatDraft, closeChat, composeChat, decoder, errorToString, executeEvent, getChatDraft, getId, getTimesoutAt, hasChat, isResumable, isSetCompleted, isUpdating, moveSelectedBack, moveSelectedBy, nextRoundStartedTimeout, openChat, roundNextEvent, selectDomino, setConnectionStatus, setNextGame, setViewport, slug, stageEvent, switchToNextGame, switchToNextRoundGame, updateDecoder, view

-}

import Chat exposing (Chat)
import ConnectionStatus exposing (ConnectionStatus)
import Css exposing (auto, backgroundColor, batch, hex, margin, property)
import Game.Board as Board exposing (Board)
import Game.Dimensions exposing (Dimensions)
import Game.Direction exposing (Direction(..))
import Game.Domino exposing (Domino)
import Game.Domino.Highlighter exposing (Highlighter)
import Game.Domino.Play as Play
import Game.End exposing (End(..))
import Game.Event as Event exposing (Event)
import Game.Id
import Game.Player as GamePlayer exposing (GamePlayer, decoder)
import Game.Player.Message exposing (Message)
import Game.Player.Presence exposing (Presence(..))
import Game.State as State exposing (State)
import Game.Type as Type exposing (GameType(..))
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (class)
import Json.Decode as Decode
    exposing
        ( Decoder
        , andThen
        , bool
        , fail
        , index
        , int
        , list
        , nullable
        , string
        , succeed
        )
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import NonEmptyList exposing (NonEmptyList)
import Player as Player exposing (Msg, Name, Player)
import Score exposing (decoder)
import Svg.Styled.Attributes exposing (css)
import UI.Button as Button
import UI.Modal as Modal


{-| Represents a Game
-}
type Game
    = Game Model


type alias Model =
    { updating : Bool
    , me : Player Name
    , viewport : Viewport
    , connectionStatus : ConnectionStatus
    , id : Game.Id.Id
    , chat : Maybe Chat
    , timeout : Maybe Int
    , timesoutAt : Maybe Int
    , winner : Maybe (Player Name)
    , events : List Event
    , state : State
    , players : List GamePlayer
    , resumable : Bool
    , board : Board
    , round : Int
    , type_ : GameType
    , message : Maybe String
    , nextGame : Maybe Game
    , selected : Maybe Domino
    }


type alias Viewport =
    { width : Float
    , height : Float
    }


create : Game.Id.Id -> Viewport -> Game
create gameId viewport =
    Game
        { updating = False
        , me = Player.guest
        , viewport = viewport
        , connectionStatus = ConnectionStatus.unknown
        , id = gameId
        , chat = Nothing
        , timeout = Nothing
        , timesoutAt = Nothing
        , winner = Nothing
        , events = []
        , state = State.active
        , players = []
        , resumable = False
        , board = Board.empty { rows = 5, columns = 5 }
        , round = 0
        , type_ = CutThroat
        , message = Nothing
        , nextGame = Nothing
        , selected = Nothing
        }


type Error
    = Board Board.Error


{-| Move the selected domino by the delta provided

    moveSelectedBy ( 3, 4 ) game == game

-}
moveSelectedBy : ( Float, Float ) -> Game -> Game
moveSelectedBy delta ((Game ({ selected, players } as model)) as game) =
    let
        moveBy domino =
            Game { model | players = players |> List.map (GamePlayer.moveDominoBy domino delta) }
    in
    selected
        |> Maybe.map moveBy
        |> Maybe.withDefault game


{-| Push selected back in the hand

    moveSelectedBack game == game

-}
moveSelectedBack : Game -> Game
moveSelectedBack ((Game ({ selected, players } as model)) as game) =
    let
        move domino =
            Game { model | players = players |> List.map (GamePlayer.moveDominoBack domino) }
    in
    selected
        |> Maybe.map move
        |> Maybe.withDefault game


{-| Returns the game id

    getId game == 3

-}
getId : Game -> Game.Id.Id
getId (Game { id }) =
    id


{-| Returns the game slug useful for building a url

    slug game == "3"

-}
slug : Game -> String
slug =
    getId


{-| Returns the next event for the current round

    roundNextEvent game == Just event

-}
roundNextEvent : Game -> Maybe Event
roundNextEvent (Game { nextGame, round, events }) =
    nextGame
        |> Maybe.andThen
            (\(Game next) ->
                if round == next.round then
                    next.events |> List.drop (List.length events) |> List.head

                else
                    Nothing
            )


{-| Returns the timeout if a next round is just started

    nextRoundStartedTimeout game == Just 25

-}
nextRoundStartedTimeout : Game -> Maybe Int
nextRoundStartedTimeout (Game { round, nextGame, timeout }) =
    nextGame
        |> Maybe.andThen
            (\(Game next) ->
                if round /= next.round then
                    timeout

                else
                    Nothing
            )


{-| Switch to the next game
-}
switchToNextGame : Game -> Game
switchToNextGame =
    switchToRoundNextGame False


{-| Switch to the next round
-}
switchToNextRoundGame : Game -> Game
switchToNextRoundGame =
    switchToRoundNextGame True


switchToRoundNextGame : Bool -> Game -> Game
switchToRoundNextGame notSameRound ((Game { chat, round, nextGame }) as game) =
    nextGame
        |> Maybe.map
            (\((Game next) as theNextGame) ->
                if xor notSameRound (round == next.round) then
                    case ( next.chat, chat ) of
                        ( Just from, Just to ) ->
                            Game { next | chat = { from = from, to = to } |> Chat.copyMessages |> Just }

                        ( _, _ ) ->
                            theNextGame

                else
                    game
            )
        |> Maybe.withDefault game


{-| Stage the event
-}
stageEvent : Event -> Game -> Game
stageEvent event (Game ({ players } as model)) =
    Game
        { model
            | updating = True
            , players = players |> List.map (GamePlayer.setWaiting >> GamePlayer.stageEvent event)
        }


{-| Execute the event staged
-}
executeEvent : Event -> Game -> Result Error Game
executeEvent event (Game ({ players, board, events } as model)) =
    board
        |> Board.executeEvent event
        |> Result.map
            (\newBoard ->
                Game
                    { model
                        | players = players |> List.map (GamePlayer.executeEvent event)
                        , board = newBoard
                        , events = events ++ [ event ]
                    }
            )
        |> Result.mapError Board


{-| So we know if the game is currently updating.
This is useful if we want to know if we can apply updates just received
-}
isUpdating : Game -> Bool
isUpdating (Game { updating }) =
    updating


{-| Can this game be resumed? Useful in situations where the game has been dormant and the first player opens the game
-}
isResumable : Game -> Bool
isResumable (Game { resumable }) =
    resumable


{-| Is this game set completed?
-}
isSetCompleted : Game -> Bool
isSetCompleted (Game { state }) =
    state |> State.isSetCompleted


{-| Returns the timeout for the current game
-}
getTimesoutAt : Game -> Maybe Int
getTimesoutAt (Game { timesoutAt }) =
    timesoutAt


{-| Select a domino for play
-}
selectDomino : Domino -> List End -> Game -> Result Error ( Game, NonEmptyList Highlighter )
selectDomino domino ends (Game ({ me, players, board } as model)) =
    let
        plays =
            ends |> List.map (\end -> Play.create me domino end)
    in
    board
        |> Board.select plays
        |> Result.map
            (\( newBoard, highlighters ) ->
                ( Game
                    { model
                        | players = players |> List.map (GamePlayer.select domino)
                        , board = newBoard
                        , selected = Just domino
                    }
                , highlighters
                )
            )
        |> Result.mapError Board


{-| Sets next game update usually as a result of a new game update receieved
-}
setNextGame : Game -> Game -> Game
setNextGame theNextGame ((Game ({ nextGame } as m)) as game) =
    if theNextGame == game then
        game

    else
        case nextGame of
            Just otherGame ->
                Game { m | nextGame = Just (otherGame |> setNextGame theNextGame) }

            Nothing ->
                Game { m | nextGame = Just theNextGame }


{-| Opens the game chat
-}
openChat : Game -> Game
openChat (Game ({ chat } as model)) =
    Game { model | chat = chat |> Maybe.map Chat.open }


{-| close previously opened chat
-}
closeChat : Game -> Game
closeChat (Game ({ chat } as model)) =
    Game { model | chat = chat |> Maybe.map Chat.close }


{-| Does this game have chat feature?
-}
hasChat : Game -> Bool
hasChat (Game { chat }) =
    chat /= Nothing


{-| Updates the draft of the chat being composed
-}
composeChat : String -> Game -> Game
composeChat draft (Game ({ chat } as model)) =
    Game { model | chat = chat |> Maybe.map (Chat.compose draft) }


{-| Returns the current chat draft being typed
-}
getChatDraft : Game -> Maybe String
getChatDraft (Game { chat }) =
    chat |> Maybe.andThen Chat.getDraft


{-| clear the draft being composed usually as a result of a message being sent
-}
clearChatDraft : Game -> Game
clearChatDraft (Game ({ chat } as model)) =
    Game { model | chat = chat |> Maybe.map Chat.clearDraft }


{-| adds a new chat message to the player chat
-}
addChatMessage : Message -> Game -> Game
addChatMessage chatMessage (Game ({ chat } as model)) =
    Game { model | chat = chat |> Maybe.map (Chat.addMessage chatMessage) }


{-| Sets the connection status of the user so the user can be alerted
-}
setConnectionStatus : ConnectionStatus -> Game -> Game
setConnectionStatus connectionStatus (Game m) =
    Game { m | connectionStatus = connectionStatus }


{-| Sets the viewport based on browser changes
-}
setViewport : Maybe Viewport -> Game -> Game
setViewport viewport ((Game ({ board } as m)) as game) =
    case viewport of
        Just aViewport ->
            Game
                { m
                    | viewport = aViewport
                    , board = board |> Board.setDimensions (aViewport |> boardDimensions)
                }

        Nothing ->
            game


boardDimensions : Viewport -> Dimensions
boardDimensions _ =
    { rows = 20, columns = 40 }



-- DECODERS


{-| Decodes an update of an existing game from one that was already decoded
-}
updateDecoder : Game -> Decoder Game
updateDecoder (Game { me, viewport, connectionStatus }) =
    decoder me viewport connectionStatus


{-| Decodes a game from JSON representation

    decoder (Me "nexus") { width = 100, height = 200, ...} Online == <Decoder Game>

-}
decoder : Player Name -> Viewport -> ConnectionStatus -> Decoder Game
decoder me viewport connectionStatus =
    succeed
        (\winner events ->
            case events |> (List.filterMap Event.getPlay >> Board.create (viewport |> boardDimensions)) of
                Ok board ->
                    succeed (Model False me viewport connectionStatus)
                        |> required "id" string
                        |> custom (Chat.decoder me)
                        |> optional "timeout_in_seconds" (nullable int) Nothing
                        |> optional "timesout_at" (nullable int) Nothing
                        |> hardcoded winner
                        |> hardcoded events
                        |> custom (State.decoder winner)
                        |> required "players" (playerDecoder me winner events)
                        |> required "resumable" bool
                        |> hardcoded board
                        |> required "round" int
                        |> required "type" (string |> andThen Type.decoder)
                        |> optional "message" (nullable string) Nothing
                        |> hardcoded Nothing
                        |> hardcoded Nothing

                Err error ->
                    fail (error |> Board.errorToString)
        )
        |> optional "winner" (Player.decoder me |> nullable) Nothing
        |> optional "plays" (list <| Event.decoder me) []
        |> andThen (Decode.map Game)


playerDecoder : Player Name -> Maybe (Player Name) -> List Event -> Decoder (List GamePlayer)
playerDecoder me winner events =
    let
        gamePlayerDecoder =
            GamePlayer.decoder me winner events
    in
    Decode.map4 (\south east north west -> [ south, east, north, west ])
        (index 0 <| gamePlayerDecoder South)
        (index 1 <| gamePlayerDecoder East)
        (index 2 <| gamePlayerDecoder North)
        (index 3 <| gamePlayerDecoder West)



-- VIEW


{-| Produced the HTML representation of the game

    view game == "<game-html>"

-}
view : Game -> Html (Msg Domino)
view ((Game { state, players, connectionStatus, board, chat }) as m) =
    div [ class "game game--full" ]
        ([ notOnlineWarningView m
         , connectionStatus |> ConnectionStatus.view
         , div [ class "game__status" ]
            [ div [ class "game-message" ] [ state |> State.view players ]
            ]
         , div [ class "game__top-left" ] [ chat |> Maybe.map Chat.view |> Maybe.withDefault (text "") ]
         , div [ class "game__top-right" ] [ div [ class "close" ] [ closeButtonView ] ]
         , div [ class "game__bottom-right" ]
            [ div [ class "next-player-action" ] [ nextActionView m ]
            ]
         , board |> Board.view
         ]
            ++ (players |> List.map GamePlayer.view)
        )


view2 : Game -> Html (Msg Domino)
view2 _ =
    div
        [ css
            [ margin auto
            , backgroundColor (hex "55af6a")
            , batch
                [ property "display" "grid"
                , property "grid-template-columns" <|
                    "repeat("
                        ++ String.fromInt 40
                        ++ ", 1.6vmax)"
                , property "grid-template-rows" <|
                    "repeat("
                        ++ String.fromInt 40
                        ++ ", 1.6vmax)"
                , property "gap" "0.1vmax"
                ]
            ]
        ]
        [ text "blah" ]



-- div [] [ "Game2" |> text, board |> Board.view ]
-- grid : List (Html (Msg a)) -> Dimensions -> Html (Msg a)
-- grid elements dimensions =
--     div
--         [ css
--             [ margin auto
--             , batch
--                 [ property "display" "grid"
--                 , property "grid-template-columns" <|
--                     "repeat("
--                         ++ String.fromInt dimensions.columns
--                         ++ ", 1.6vmax)"
--                 , property "grid-template-rows" <|
--                     "repeat("
--                         ++ String.fromInt dimensions.rows
--                         ++ ", 1.6vmax)"
--                 , property "gap" "0.1vmax"
--                 ]
--             ]
--         ]
--         elements


closeButtonView : Html (Msg Domino)
closeButtonView =
    Button.danger
        { action = Player.ClosedGame
        , label = "X"
        }
        |> Button.view


nextActionView : Game -> Html (Msg Domino)
nextActionView (Game { state, timeout, players }) =
    if State.isActive state then
        if timeout /= Nothing && presenceForMe players == Online then
            Button.danger
                { action = Player.GoesAway
                , label = "BRB"
                }
                |> Button.view

        else
            text ""

    else if State.isCompleted state then
        Button.primary
            { action = Player.SwitchedToNextGame
            , label = "Next game"
            }
            |> Button.view

    else if State.isSetCompleted state then
        Button.cta
            { action = Player.ClosedGame
            , label =
                case players |> List.filter GamePlayer.isMe |> List.head of
                    Nothing ->
                        "Watch next"

                    Just _ ->
                        "Play again"
            }
            |> Button.view

    else
        text ""


presenceForMe : List GamePlayer -> Presence
presenceForMe players =
    players
        |> List.filter GamePlayer.isMe
        |> List.head
        |> Maybe.map GamePlayer.getPresence
        |> Maybe.withDefault Offline


notOnlineWarningView : Game -> Html (Msg Domino)
notOnlineWarningView (Game { players, state }) =
    let
        presence =
            presenceForMe players

        playing =
            players |> List.any GamePlayer.isMe

        buttons =
            [ Button.cta
                { action = Player.ComesBack
                , label = "Resume"
                }
            ]
    in
    if state |> State.isSetCompleted then
        text ""

    else if playing && presence == Away then
        Modal.create
            { title = "You are away..."
            , body = text "If you are back, select Resume, otherwise select Watch to just watch for now."
            , onClose = Player.ClosedGame
            , buttons = buttons
            }
            |> Modal.open
            |> Modal.view

    else if playing && presence == Offline then
        Modal.create
            { title = "You went offline..."
            , body = text "Click resume if you are back or watch until you are ready."
            , onClose = Player.ClosedGame
            , buttons = buttons
            }
            |> Modal.open
            |> Modal.view

    else
        text ""


{-| Returns the string representation of the game error
-}
errorToString : Error -> String
errorToString error =
    case error of
        Board err ->
            err |> Board.errorToString
