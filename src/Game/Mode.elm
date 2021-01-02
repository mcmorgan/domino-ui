module Game.Mode exposing
    ( Mode(..)
    , decoder
    )

import Json.Decode exposing (Decoder, fail, succeed)


type Mode
    = Live
    | Practice


decoder : String -> Decoder Mode
decoder s =
    case s of
        "Domino::PartnerLiveGame" ->
            succeed Live

        "Domino::CutThroatLiveGame" ->
            succeed Live

        "Domino::CutThroatPracticeGame" ->
            succeed Practice

        _ ->
            fail ("Unknown game type: " ++ s)
