module ChatTest exposing (suite)

import Chat as Chat exposing (decoder)
import Expect
import Game.Player.Message as Message
import Json.Decode as Decode
import Player as Player
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "Chat"
        [ decoderTest
        , addMessageTest
        ]


addMessageTest : Test
addMessageTest =
    describe "addMessage"
        [ test "prevents duplicates" <|
            \_ ->
                [ { id = 1, read = True, player = Player.me "one", message = "first", sentAt = Time.millisToPosix 1 }
                , { id = 2, read = True, player = Player.opponent "two", message = "second", sentAt = Time.millisToPosix 2 }
                , { id = 2, read = True, player = Player.opponent "two", message = "second", sentAt = Time.millisToPosix 2 }
                , { id = 2, read = True, player = Player.opponent "two", message = "second", sentAt = Time.millisToPosix 2 }
                , { id = 3, read = True, player = Player.opponent "three", message = "third", sentAt = Time.millisToPosix 3 }
                , { id = 3, read = True, player = Player.opponent "three", message = "third", sentAt = Time.millisToPosix 3 }
                , { id = 3, read = True, player = Player.opponent "three", message = "third", sentAt = Time.millisToPosix 3 }
                ]
                    |> List.map Message.create
                    |> List.foldl Chat.addMessage Chat.create
                    |> Expect.equal
                        ([ { id = 1, read = True, player = Player.me "one", message = "first", sentAt = Time.millisToPosix 1 }
                         , { id = 2, read = True, player = Player.opponent "two", message = "second", sentAt = Time.millisToPosix 2 }
                         , { id = 3, read = True, player = Player.opponent "three", message = "third", sentAt = Time.millisToPosix 3 }
                         ]
                            |> List.map Message.create
                            |> List.foldl Chat.addMessage Chat.create
                        )
        ]


decoderTest : Test
decoderTest =
    let
        expectedOutput =
            [ { id = 1, read = True, player = Player.me "one", message = "first", sentAt = Time.millisToPosix 1 }
            , { id = 2, read = True, player = Player.opponent "two", message = "second", sentAt = Time.millisToPosix 2 }
            , { id = 3, read = True, player = Player.opponent "three", message = "third", sentAt = Time.millisToPosix 3 }
            ]
                |> List.map Message.create
                |> List.foldl Chat.addMessage Chat.create
                |> Just
                |> Ok
    in
    describe "decoder"
        [ test "decodes correctly" <|
            \_ ->
                """
                {
                    "chat": true,
                    "chats": [
                        { "id": 3, "player_name": "three", "message": "third", "sent_at": 3 },
                        { "id": 2, "player_name": "two", "message": "second", "sent_at": 2 },
                        { "id": 1, "player_name": "one", "message": "first", "sent_at": 1 }
                    ]
                }
                """
                    |> Decode.decodeString (decoder (Player.me "one"))
                    |> Expect.equal expectedOutput
        , test "does not include duplicates" <|
            \_ ->
                """
                {
                    "chat": true,
                    "chats": [
                        { "id": 3, "player_name": "three", "message": "third", "sent_at": 3 },
                        { "id": 2, "player_name": "two", "message": "second", "sent_at": 2 },
                        { "id": 1, "player_name": "one", "message": "first", "sent_at": 1 },
                        { "id": 1, "player_name": "one", "message": "first", "sent_at": 1 },
                        { "id": 1, "player_name": "one", "message": "first", "sent_at": 1 }
                    ]
                }
                """
                    |> Decode.decodeString (decoder (Player.me "one"))
                    |> Expect.equal expectedOutput
        ]
