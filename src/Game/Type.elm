module Game.Type exposing
    ( GameType(..)
    , decoder
    )

import Json.Decode exposing (Decoder, fail, succeed)


type GameType
    = CutThroat
    | Partner


decoder : String -> Decoder GameType
decoder s =
    case s of
        "partner" ->
            succeed Partner

        "cut_throat" ->
            succeed CutThroat

        "Domino::PartnerLiveGame" ->
            succeed Partner

        "Domino::CutThroatLiveGame" ->
            succeed CutThroat

        "Domino::CutThroatPracticeGame" ->
            succeed CutThroat

        _ ->
            fail ("Unknown game type: " ++ s)
