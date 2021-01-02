module Score exposing
    ( Score(..)
    , decoder
    , individual
    , team
    , view
    )

import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder, int, oneOf)
import Json.Decode.Pipeline exposing (required)


type alias Value =
    Int


type Score
    = Individual Value
    | Team Value Value


individual : Value -> Score
individual =
    Individual


team : Value -> Value -> Score
team =
    Team



-- DECODERS


decoder : Decoder Score
decoder =
    oneOf
        [ teamDecoder
        , individualDecoder
        ]


teamDecoder : Decoder Score
teamDecoder =
    Decode.succeed Team
        |> required "score" int
        |> required "team_score" int


individualDecoder : Decoder Score
individualDecoder =
    Decode.succeed Individual
        |> required "score" int



-- VIEW


view : Score -> Html msg
view s =
    div [ class "score" ]
        [ case s of
            Individual i ->
                text <| String.fromInt i

            Team i t ->
                text <| String.join "-" <| List.map String.fromInt [ i, t ]
        ]
