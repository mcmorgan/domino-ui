module ConnectionStatus exposing
    ( ConnectionStatus
    , decoder
    , isOnline
    , isReconnect
    , online
    , unknown
    , view
    )

import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (classList)
import Json.Decode exposing (Decoder, andThen, bool, succeed)


type ConnectionStatus
    = Unknown
    | Online
    | Offline


online : ConnectionStatus
online =
    Online


isOnline : ConnectionStatus -> Bool
isOnline c =
    case c of
        Online ->
            True

        _ ->
            False


unknown : ConnectionStatus
unknown =
    Unknown


isReconnect : ConnectionStatus -> ConnectionStatus -> Bool
isReconnect old new =
    case ( old, new ) of
        ( Offline, Online ) ->
            True

        _ ->
            False


decoder : Decoder ConnectionStatus
decoder =
    bool
        |> andThen
            (\connected ->
                succeed <|
                    if connected then
                        Online

                    else
                        Offline
            )


view : ConnectionStatus -> Html msg
view cs =
    let
        ( message, class_ ) =
            case cs of
                Unknown ->
                    ( "Connecting...", "connection--status-connecting" )

                Offline ->
                    ( "You are offline", "connection--status-offline" )

                Online ->
                    ( "You are online", "connection--status-online" )
    in
    div
        [ classList
            [ ( "connection-status", True )
            , ( class_, True )
            ]
        ]
        [ text message
        ]
