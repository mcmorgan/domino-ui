module Game.Id exposing
    ( Id
    , decoder
    , encode
    , toString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Id =
    String


decoder : Decoder Id
decoder =
    Decode.string


encode : Id -> Encode.Value
encode =
    Encode.string


toString : Id -> String
toString id =
    id
