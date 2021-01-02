module Game.Fuzzer exposing
    ( end
    , suit
    )

import Fuzz as Fuzz exposing (Fuzzer)
import Game.Domino.Suit exposing (Suit(..))
import Game.End exposing (End(..))


suit : Fuzzer Suit
suit =
    Fuzz.oneOf
        [ Fuzz.constant Blank
        , Fuzz.constant Ace
        , Fuzz.constant Deuce
        , Fuzz.constant Trey
        , Fuzz.constant Four
        , Fuzz.constant Five
        , Fuzz.constant Six
        ]


end : Fuzzer End
end =
    Fuzz.oneOf
        [ Fuzz.constant Left
        , Fuzz.constant Right
        ]
