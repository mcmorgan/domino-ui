module Game.Id exposing
    ( GameId
    , decoder
    , encode
    , toString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias GameId =
    Int


decoder : Decoder GameId
decoder =
    Decode.int


encode : GameId -> Encode.Value
encode id =
    Encode.int id


toString : GameId -> String
toString =
    String.fromInt
