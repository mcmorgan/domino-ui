module Game.End exposing
    ( End(..)
    , decoder
    , encode
    , opposite
    , toString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type End
    = Left
    | Right


decoder : String -> Decoder End
decoder s =
    case s of
        "left" ->
            Decode.succeed Left

        "right" ->
            Decode.succeed Right

        _ ->
            Decode.fail <| "Invalid end " ++ s


encode : End -> Encode.Value
encode e =
    Encode.string <| toString <| e


opposite : End -> End
opposite e =
    case e of
        Left ->
            Right

        Right ->
            Left


toString : End -> String
toString e =
    case e of
        Left ->
            "left"

        Right ->
            "right"
