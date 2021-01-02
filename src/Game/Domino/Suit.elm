module Game.Domino.Suit exposing
    ( Suit(..)
    , decoder
    , different
    , encode
    , toInt
    , toString
    , view
    )

import Css exposing (batch, padding, property, vmax)
import Game.Direction as Direction exposing (Direction(..))
import Game.Domino.Pip as Pip exposing (Pip)
import Game.Orientation exposing (Orientation(..))
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import UI.Theme as Theme


type Suit
    = Blank
    | Ace
    | Deuce
    | Trey
    | Four
    | Five
    | Six


decoder : Decoder Suit
decoder =
    let
        toSuit i =
            case i of
                0 ->
                    Decode.succeed Blank

                1 ->
                    Decode.succeed Ace

                2 ->
                    Decode.succeed Deuce

                3 ->
                    Decode.succeed Trey

                4 ->
                    Decode.succeed Four

                5 ->
                    Decode.succeed Five

                6 ->
                    Decode.succeed Six

                _ ->
                    Decode.fail <| "Invalid suit number: " ++ String.fromInt i
    in
    Decode.int
        |> Decode.andThen toSuit


encode : Suit -> Encode.Value
encode =
    toInt >> Encode.int


toInt : Suit -> Int
toInt suit =
    case suit of
        Blank ->
            0

        Ace ->
            1

        Deuce ->
            2

        Trey ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6


toString : Suit -> String
toString =
    toInt >> String.fromInt


different : Suit -> Suit
different s =
    case s of
        Blank ->
            Ace

        Ace ->
            Deuce

        Deuce ->
            Trey

        Trey ->
            Four

        Four ->
            Five

        Five ->
            Six

        Six ->
            Blank


pipsOn : Direction -> Suit -> List Pip
pipsOn direction suit =
    case ( suit, direction |> Direction.toOrientation ) of
        ( Blank, _ ) ->
            []

        ( Ace, _ ) ->
            [ Pip.Five ]

        ( Deuce, Horizontal ) ->
            [ Pip.Three
            , Pip.Seven
            ]

        ( Deuce, Vertical ) ->
            [ Pip.One
            , Pip.Nine
            ]

        ( Trey, _ ) ->
            [ Pip.One
            , Pip.Five
            , Pip.Nine
            ]

        ( Four, _ ) ->
            [ Pip.One
            , Pip.Three
            , Pip.Seven
            , Pip.Nine
            ]

        ( Five, _ ) ->
            [ Pip.One
            , Pip.Three
            , Pip.Five
            , Pip.Seven
            , Pip.Nine
            ]

        ( Six, Horizontal ) ->
            [ Pip.One
            , Pip.Two
            , Pip.Three
            , Pip.Seven
            , Pip.Eight
            , Pip.Nine
            ]

        ( Six, Vertical ) ->
            [ Pip.One
            , Pip.Four
            , Pip.Seven
            , Pip.Three
            , Pip.Six
            , Pip.Nine
            ]


view : Direction -> Suit -> Html msg
view direction suit =
    let
        pipsForSuit =
            pipsOn direction suit
    in
    div
        [ css
            [ batch
                [ property "display" "grid"
                , property "grid-template-columns" "repeat(3, min-content)"
                , property "grid-template-rows" "repeat(3, min-content)"
                ]
            , padding (vmax (0.1 * Theme.domino.size))
            ]
        ]
        ((pipsForSuit
            |> List.map (Pip.view True)
         )
            ++ (List.filter (\x -> List.member x pipsForSuit |> not) Pip.all
                    |> List.map (Pip.view False)
               )
        )
