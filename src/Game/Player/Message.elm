module Game.Player.Message exposing
    ( Message
    , create
    , decoder
    , isEqual
    , markRead
    , unread
    , view
    )

import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder, andThen, int, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Player as Player exposing (Name, Player)
import Time


type Message
    = Message Model


type alias Model =
    { id : Int
    , player : Player Name
    , message : String
    , read : Bool
    , sentAt : Time.Posix
    }


create : Model -> Message
create =
    Message


unread : Message -> Bool
unread (Message { read }) =
    not read


markRead : Message -> Message
markRead (Message m) =
    Message { m | read = True }


isEqual : Message -> Message -> Bool
isEqual (Message a) (Message b) =
    a.id == b.id



-- DECODER


decoder : Bool -> Player Name -> Decoder Message
decoder read me =
    let
        decodeTime =
            Decode.int |> Decode.andThen (\t -> succeed <| Time.millisToPosix t)
    in
    succeed Model
        |> required "id" int
        |> required "player_name" (Player.decoder me)
        |> required "message" string
        |> hardcoded read
        |> required "sent_at" decodeTime
        |> Decode.map Message



-- VIEW


view : Message -> Html msg
view (Message { player, message }) =
    div [ class "chat" ]
        [ div [ class "chat__player" ]
            [ text <| Player.toName player
            ]
        , div [ class "chat__message" ] [ text message ]
        ]
