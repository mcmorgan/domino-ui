module Player exposing
    ( Msg(..)
    , Name
    , Player
    , current
    , decoder
    , encode
    , fromString
    , guest
    , isMe
    , isOffline
    , me
    , opponent
    , toName
    , view
    )

import Draggable
import Game.End exposing (End(..))
import Game.Id exposing (GameId)
import Game.Player.Presence as Presence exposing (Presence)
import Game.Type exposing (GameType)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (classList)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Name =
    String


type Player name
    = Me name
    | Opponent name
    | Guest


type Msg domino
    = Selected ( domino, List End )
    | DragMsg (Draggable.Msg ( domino, List End ))
    | Dragging Draggable.Delta
    | DragStarted ( domino, List End )
    | DragEnded
    | Played domino End
    | ComesBack
    | GoesAway
    | ClosedGame
    | SwitchedToNextGame
    | OpenedGame GameId
    | JoinedGame GameType
    | LeftGame GameType
    | PlayedPractice GameType
    | OpenedChat
    | ClosedChat
    | ChangedMessage String
    | SentChatMessage


current : Maybe String -> Player Name
current s =
    case s of
        Just name ->
            Me name

        Nothing ->
            Guest


encode : Player Name -> Encode.Value
encode pn =
    Encode.string <| toName <| pn


toName : Player Name -> Name
toName player =
    case player of
        Opponent username ->
            username

        Me username ->
            username

        Guest ->
            "guest"


me : Name -> Player Name
me =
    Me


guest : Player Name
guest =
    Guest


opponent : Name -> Player Name
opponent =
    Opponent


isMe : Player a -> Bool
isMe p =
    case p of
        Me _ ->
            True

        _ ->
            False


isOffline : List (Player a) -> Player a -> Bool
isOffline connectedPlayers player =
    not <| isMe player || List.member player connectedPlayers


decoder : Player Name -> Decoder (Player Name)
decoder p =
    Decode.string
        |> Decode.andThen
            (Decode.succeed << fromString p)


fromString : Player Name -> Name -> Player Name
fromString p name =
    if Me name == p then
        p

    else
        Opponent name


view : Presence -> Player Name -> Html msg
view presence p =
    div
        [ classList
            [ ( "player", True )
            , ( "player__current", isMe p )
            , ( "player--status-" ++ Presence.toString presence, True )
            ]
        ]
        [ text <| toName p
        ]
