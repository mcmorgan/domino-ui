module Game.Player.Presence exposing
    ( Presence(..)
    , decoder
    , toString
    )

import Json.Decode exposing (Decoder, andThen, fail, string, succeed)


type Presence
    = Offline
    | Online
    | Away



-- DECODER


decoder : Decoder Presence
decoder =
    string
        |> andThen
            (\s ->
                case s of
                    "online" ->
                        succeed Online

                    "offline" ->
                        succeed Offline

                    "away" ->
                        succeed Away

                    _ ->
                        fail ("Unknown player state: " ++ s)
            )


toString : Presence -> String
toString presence =
    case presence of
        Online ->
            "online"

        Offline ->
            "offline"

        Away ->
            "away"
