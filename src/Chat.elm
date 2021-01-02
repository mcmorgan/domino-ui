module Chat exposing
    ( Chat
    , addMessage
    , clearDraft
    , close
    , compose
    , copyMessages
    , create
    , decoder
    , getDraft
    , open
    , view
    )

import Game.Domino exposing (Domino)
import Game.Player.Message as Message exposing (Message)
import Html.Styled exposing (Html, div, input)
import Html.Styled.Attributes exposing (class, placeholder, value)
import Html.Styled.Events exposing (keyCode, on, onInput)
import Json.Decode as Decode exposing (Decoder, andThen, bool, fail, field, list, succeed)
import Json.Decode.Pipeline exposing (required)
import Player exposing (Msg(..), Name, Player)
import UI.Button as Button exposing (Button(..), view)
import UI.Modal as Modal


type Chat
    = Chat Model


type alias Model =
    { state : State
    , composing : Maybe String
    , messages : List Message
    }


type State
    = Opened
    | Closed


create : Chat
create =
    Chat
        { state = Closed
        , composing = Nothing
        , messages = []
        }


open : Chat -> Chat
open (Chat model) =
    Chat { model | state = Opened }


close : Chat -> Chat
close (Chat ({ messages } as model)) =
    Chat { model | state = Closed, messages = messages |> List.map Message.markRead }


compose : String -> Chat -> Chat
compose draft (Chat model) =
    Chat { model | composing = Just draft }


getDraft : Chat -> Maybe String
getDraft (Chat { composing }) =
    composing


clearDraft : Chat -> Chat
clearDraft (Chat model) =
    Chat { model | composing = Nothing }


addMessage : Message -> Chat -> Chat
addMessage message ((Chat ({ messages } as model)) as chat) =
    if messages |> List.any (Message.isEqual message) then
        chat

    else
        Chat { model | messages = message :: messages }


copyMessages : { from : Chat, to : Chat } -> Chat
copyMessages { from, to } =
    case ( from, to ) of
        ( Chat { messages }, Chat toModel ) ->
            Chat { toModel | messages = messages }


decoder : Player Name -> Decoder (Maybe Chat)
decoder me =
    let
        enabledDecoder =
            Decode.succeed (List.foldr addMessage create)
                |> required "chats" (Message.decoder True me |> list)
                |> Decode.map Just

        disabledDecoder =
            succeed Nothing
    in
    field "chat" bool
        |> andThen
            (\enabled ->
                if enabled then
                    enabledDecoder

                else
                    disabledDecoder
            )


view : Chat -> Html (Msg Domino)
view (Chat { state, composing, messages }) =
    case state of
        Opened ->
            openedView composing messages

        Closed ->
            openButton messages


openedView : Maybe String -> List Message -> Html (Msg Domino)
openedView composing messages =
    Modal.create
        { title = "Chats"
        , body =
            div [ class "chats-remove-this-class-in-css" ]
                (input
                    [ class "compose-chat-messsage"
                    , placeholder "Type message then press enter"
                    , value <| Maybe.withDefault "" composing
                    , onInput ChangedMessage
                    , onEnter SentChatMessage
                    ]
                    []
                    :: List.map Message.view messages
                )
        , onClose = ClosedChat
        , buttons = []
        }
        |> Modal.open
        |> Modal.view


openButton : List Message -> Html (Msg Domino)
openButton messages =
    let
        unreadMessageCount =
            messages |> List.filter Message.unread |> List.length

        defaultButtonOptions =
            { action = OpenedChat
            , label = "Chat"
            }
    in
    div [ class "chat__button" ]
        [ if unreadMessageCount > 0 then
            Button.primary
                { defaultButtonOptions | label = "Chat (" ++ String.fromInt unreadMessageCount ++ ")" }
                |> Button.view

          else
            Button.secondary defaultButtonOptions |> Button.view
        ]


onEnter : msg -> Html.Styled.Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                succeed msg

            else
                fail "not ENTER"
    in
    on "keydown" (andThen isEnter keyCode)
