module Game.Domino exposing
    ( Domino
    , create
    , decoder
    , encode
    , flip
    , getDimensions
    , getDirection
    , getId
    , getSuit
    , isDouble
    , long
    , matches
    , setDirection
    , short
    , toString
    , view
    )

import Css exposing (backgroundColor)
import Game.Dimensions exposing (Dimensions)
import Game.Direction as Direction exposing (Direction(..))
import Game.Domino.Separator as Separator
import Game.Domino.Suit as Suit exposing (Suit(..))
import Game.End exposing (End(..))
import Game.Orientation exposing (Orientation(..))
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (class, css)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Player
import UI.Theme as Theme


type Domino
    = Domino Model


type alias Unit =
    Int


type alias Model =
    { left : Suit
    , right : Suit
    , direction : Direction
    }


getDirection : Domino -> Direction
getDirection (Domino { direction }) =
    direction


setDirection : Direction -> Domino -> Domino
setDirection direction (Domino m) =
    Domino { m | direction = direction }


getDimensions : Domino -> Dimensions
getDimensions (Domino { direction }) =
    case direction |> Direction.toOrientation of
        Horizontal ->
            { columns = long, rows = short }

        Vertical ->
            { columns = short, rows = long }


short : Unit
short =
    2


long : Unit
long =
    short * 2


create : Suit -> Suit -> Domino
create left right =
    Model left right East |> Domino


decoder : Decoder Domino
decoder =
    Decode.map2 create
        (Decode.index 0 Suit.decoder)
        (Decode.index 1 Suit.decoder)


encode : Domino -> Encode.Value
encode d =
    [ Left, Right ]
        |> List.map (getSuit d)
        |> Encode.list Suit.encode


flip : Domino -> Domino
flip (Domino ({ left, right } as m)) =
    Domino { m | right = left, left = right }


matches : Domino -> Domino -> Bool
matches (Domino a) (Domino b) =
    (a.left == b.left && a.right == b.right)
        || (a.left == b.right && a.right == b.left)


getSuit : Domino -> End -> Suit
getSuit (Domino { left, right }) end =
    case end of
        Left ->
            left

        Right ->
            right


isDouble : Domino -> Bool
isDouble (Domino { left, right }) =
    left == right


getId : Domino -> String
getId (Domino { left, right }) =
    [ left, right ] |> List.map Suit.toString |> String.join "-"


toString : Domino -> String
toString (Domino { left, right }) =
    "[" ++ Suit.toString left ++ "," ++ Suit.toString right ++ "]"



-- VIEWS


view : Domino -> Html (Player.Msg Domino)
view (Domino { left, right, direction }) =
    let
        body =
            [ Suit.view direction left
            , Separator.view direction
            , Suit.view direction right
            ]
    in
    div
        [ class <| "domino" ++ "-" ++ Suit.toString left ++ Suit.toString right
        , css
            (backgroundColor Theme.domino.bodyColor
                :: Theme.domino.defaultStyles
                ++ Direction.styles direction
            )
        ]
        (case direction of
            North ->
                body |> List.reverse

            East ->
                body

            West ->
                body |> List.reverse

            South ->
                body
        )
