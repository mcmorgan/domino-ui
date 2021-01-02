module Game.Domino.Playable exposing
    ( Playable
    , create
    , getModel
    , isEqual
    , moveBack
    , moveBy
    , reset
    , view
    )

import Draggable
import Game.Direction exposing (Direction(..))
import Game.Domino as Domino exposing (Domino)
import Game.Domino.Suit exposing (Suit(..))
import Game.End exposing (End)
import Game.Orientation exposing (Orientation(..))
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes as Attributes exposing (id, style)
import Player exposing (Msg)


type Playable
    = Playable ( Float, Float ) Model


type alias Model =
    { ends : List End
    , direction : Direction
    , domino : Domino
    }


create : Model -> Playable
create =
    Playable origin


origin : ( Float, Float )
origin =
    ( 0, 0 )


reset : Playable -> Playable
reset (Playable _ model) =
    Playable origin model


moveBy : ( Float, Float ) -> Playable -> Playable
moveBy ( dx, dy ) (Playable ( x, y ) model) =
    Playable ( x + dx, y + dy ) model


moveBack : Playable -> Playable
moveBack (Playable _ model) =
    Playable origin model


getModel : Playable -> Model
getModel (Playable _ m) =
    m


isEqual : Playable -> Playable -> Bool
isEqual (Playable _ one) (Playable _ other) =
    Domino.matches one.domino other.domino
        && one.ends
        == other.ends
        && one.direction
        == other.direction


view : Playable -> Html (Msg Domino)
view (Playable ( x, y ) { domino, direction, ends }) =
    let
        jumpBy =
            25

        ( sx, sy ) =
            case direction of
                North ->
                    ( 0, jumpBy )

                South ->
                    ( 0, -jumpBy )

                East ->
                    ( -jumpBy, 0 )

                West ->
                    ( jumpBy, 0 )
    in
    div
        ([ id <| (domino |> Domino.getId)
         , style "transform" ("translate(" ++ String.fromFloat (x + sx) ++ "px, " ++ String.fromFloat (sy + y) ++ "px)")
         , Draggable.mouseTrigger ( domino, ends ) Player.DragMsg |> Attributes.fromUnstyled
         ]
            ++ (if ( x, y ) /= origin then
                    [ style "z-index" "1000" ]

                else
                    []
               )
            ++ (Draggable.touchTriggers ( domino, ends ) Player.DragMsg |> List.map Attributes.fromUnstyled)
        )
        [ domino |> Domino.view
        ]
