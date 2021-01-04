module Game.Hand exposing
    ( Hand
    , HandDomino(..)
    , addExposed
    , addPlayed
    , addUnexposed
    , create
    , decoder
    , expose
    , getDirection
    , getPlayable
    , moveDominoBack
    , moveDominoBy
    , remove
    , select
    , unsetTurn
    , view
    )

import Css
    exposing
        ( column
        , displayFlex
        , flexDirection
        )
import Game.Direction exposing (Direction(..))
import Game.Domino as Domino exposing (Domino)
import Game.Domino.Hidden as Hidden
import Game.Domino.Play as Play exposing (Play)
import Game.Domino.Playable as Playable exposing (Playable)
import Game.Domino.Unexposed as Unexposed exposing (Unexposed)
import Game.End exposing (End(..))
import Game.Event as Event exposing (Event)
import Game.Orientation exposing (Orientation(..))
import Game.Player.State as State exposing (State)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Json.Decode as Decode exposing (Decoder)
import Player exposing (Msg)


type Hand
    = Hand Model


type alias Model =
    { dominoes : List HandDomino
    , direction : Direction
    }


type HandDomino
    = Unselected Domino
    | Playable Playable
    | Unexposed Unexposed
    | Played Play


moveDominoBy : Domino -> ( Float, Float ) -> Hand -> Hand
moveDominoBy domino delta (Hand ({ dominoes } as model)) =
    let
        moveMatchedDominoBy handDomino =
            case handDomino of
                Playable playable ->
                    if playable |> Playable.getModel >> .domino |> Domino.matches domino then
                        playable |> Playable.moveBy delta |> Playable

                    else
                        handDomino

                _ ->
                    handDomino
    in
    Hand { model | dominoes = dominoes |> List.map moveMatchedDominoBy }


moveDominoBack : Domino -> Hand -> Hand
moveDominoBack domino (Hand ({ dominoes } as model)) =
    let
        moveMatchedDominoBy handDomino =
            case handDomino of
                Playable playable ->
                    if playable |> Playable.getModel >> .domino |> Domino.matches domino then
                        playable |> Playable.moveBack |> Playable

                    else
                        handDomino

                _ ->
                    handDomino
    in
    Hand { model | dominoes = dominoes |> List.map moveMatchedDominoBy }


getDirection : Hand -> Direction
getDirection (Hand { direction }) =
    direction


addExposed : Domino -> Hand -> Hand
addExposed domino (Hand ({ dominoes, direction } as m)) =
    Hand { m | dominoes = dominoes ++ [ domino |> Domino.setDirection direction |> Unselected ] }


addUnexposed : Hand -> Hand
addUnexposed (Hand ({ dominoes, direction } as m)) =
    Hand { m | dominoes = dominoes ++ [ Unexposed.create direction |> Unexposed ] }


addPlayed : Play -> Hand -> Hand
addPlayed play (Hand ({ dominoes } as m)) =
    Hand { m | dominoes = dominoes ++ [ play |> Played ] }


remove : Play -> Hand -> Hand
remove play (Hand ({ dominoes } as m)) =
    let
        markPlayed handDomino =
            if matches (play |> Play.getDomino |> Domino.matches) handDomino then
                Played play

            else
                handDomino
    in
    Hand { m | dominoes = dominoes |> List.map markPlayed }


unsetTurn : Hand -> Hand
unsetTurn (Hand ({ dominoes } as m)) =
    let
        unselectPlayableOrSelected handDomino =
            case handDomino of
                Playable playable ->
                    playable |> Playable.getModel |> .domino |> Unselected

                _ ->
                    handDomino
    in
    Hand { m | dominoes = dominoes |> List.map unselectPlayableOrSelected }


expose : Play -> Hand -> Hand
expose play ((Hand ({ direction, dominoes } as m)) as hand) =
    if dominoes |> List.any (play |> Play.getDomino |> Domino.matches |> matches) then
        hand

    else
        let
            exposed =
                Unselected (Play.getDomino play |> Domino.setDirection direction)
        in
        case dominoes |> List.partition isUnexposed of
            ( unexposed, rest ) ->
                Hand { m | dominoes = (unexposed |> List.drop 1) ++ exposed :: rest }


isUnexposed : HandDomino -> Bool
isUnexposed handDomino =
    case handDomino of
        Unexposed _ ->
            True

        _ ->
            False


matches : (Domino -> Bool) -> HandDomino -> Bool
matches matcher handDomino =
    case handDomino of
        Unexposed _ ->
            False

        Played _ ->
            False

        Unselected unselecteDomino ->
            unselecteDomino |> matcher

        Playable playable ->
            playable |> Playable.getModel |> .domino |> matcher


select : Domino -> Hand -> Hand
select _ (Hand ({ dominoes } as model)) =
    let
        selectDomino d =
            -- case d of
            --     Playable playable ->
            --         case playable |> Playable.getModel of
            --             { domino } ->
            --                 if domino |> Domino.matches selectedDomino then
            --                     playable |> Playable.hide |> Playable
            --                 else
            --                     d
            --     _ ->
            d
    in
    Hand { model | dominoes = dominoes |> List.map selectDomino }


create : Direction -> Hand
create direction =
    Hand { dominoes = [], direction = direction }


decoder : Direction -> State -> List Event -> Decoder Hand
decoder direction state events =
    Decode.list (Domino.decoder |> Decode.maybe)
        |> Decode.andThen
            (\dominoes ->
                let
                    setState domino =
                        let
                            ends =
                                state
                                    |> State.getPlays
                                    |> List.filter (Play.getDomino >> Domino.matches domino)
                                    |> List.map Play.getEnd
                        in
                        if List.isEmpty ends then
                            Unselected domino

                        else
                            Playable (Playable.create { ends = ends, direction = direction, domino = domino })

                    exposed =
                        dominoes
                            |> List.filterMap identity
                            |> List.map (Domino.setDirection direction)
                            |> List.map setState

                    hidden =
                        dominoes
                            |> List.filter ((==) Nothing)
                            |> List.map (\_ -> direction |> Unexposed.create |> Unexposed)

                    played =
                        events |> List.filterMap Event.getPlay |> List.map Played
                in
                Decode.succeed <|
                    Hand
                        { dominoes = exposed ++ hidden ++ played
                        , direction = direction
                        }
            )


getPlayable : Hand -> List Playable
getPlayable (Hand { dominoes }) =
    let
        getPlayableDomino handDomino =
            case handDomino of
                Playable playable ->
                    Just playable

                _ ->
                    Nothing
    in
    dominoes |> List.filterMap getPlayableDomino


view : Hand -> Html (Msg Domino)
view (Hand { dominoes, direction }) =
    let
        dominoView handDomino =
            case handDomino of
                Unexposed hidden ->
                    Unexposed.view hidden

                Unselected domino ->
                    Domino.view domino

                Playable playable ->
                    Playable.view playable

                Played _ ->
                    Hidden.create direction |> Hidden.view

        directionStyles =
            case direction of
                North ->
                    []

                East ->
                    [ flexDirection column ]

                West ->
                    [ flexDirection column ]

                South ->
                    []
    in
    div [ css ([ displayFlex ] ++ directionStyles) ] <|
        (dominoes |> List.map dominoView)
