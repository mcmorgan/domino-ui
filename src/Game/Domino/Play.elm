module Game.Domino.Play exposing
    ( Error(..)
    , Play
    , align
    , create
    , decoder
    , encode
    , errorToString
    , getDimensions
    , getDomino
    , getEnd
    , getId
    , isForPlayer
    , mapDomino
    , matches
    , setDirection
    , setEnd
    , toMsg
    , toString
    , turnIfDouble
    , view
    )

import Game.Dimensions exposing (Dimensions)
import Game.Direction as Direction exposing (Direction(..))
import Game.Domino as Domino exposing (Domino)
import Game.Domino.Suit as Suit exposing (Suit(..))
import Game.End as End exposing (End(..))
import Game.Id as GameId exposing (GameId)
import Html.Styled exposing (Html)
import Json.Decode exposing (Decoder, andThen, field, map, map2, oneOf, string, succeed)
import Json.Encode as Encode
import Player exposing (Msg, Name, Player)


type Play
    = Play ( Player Name, Domino, End )


type Error
    = Invalid Domino End Suit


toMsg : Play -> Msg Domino
toMsg (Play ( _, domino, end )) =
    Player.Played domino end


getId : Play -> String
getId (Play ( _, domino, end )) =
    [ "play", Domino.getId domino, End.toString end ] |> String.join "-"


create : Player Name -> Domino -> End -> Play
create playerName domino end =
    Play ( playerName, domino, end )


align : Play -> Play -> Result Error Play
align (Play ( _, dominoAtEnd, _ )) (Play ( playerName, domino, end )) =
    let
        suitAtEnd =
            Domino.getSuit dominoAtEnd end
    in
    [ domino, domino |> Domino.flip ]
        |> List.filter (\alignedDomino -> suitAtEnd == Domino.getSuit alignedDomino (End.opposite end))
        |> List.head
        |> Maybe.map (\alignedDomino -> Ok (Play ( playerName, alignedDomino, end )))
        |> Maybe.withDefault
            (Err <| Invalid domino end suitAtEnd)


setEnd : End -> Play -> Play
setEnd end (Play ( playerName, domino, _ )) =
    Play ( playerName, domino, end )


getEnd : Play -> End
getEnd (Play ( _, _, end )) =
    end


getDimensions : Play -> Dimensions
getDimensions (Play ( _, domino, _ )) =
    domino |> Domino.getDimensions


getDomino : Play -> Domino
getDomino (Play ( _, domino, _ )) =
    domino


turnIfDouble : Play -> Play
turnIfDouble ((Play ( _, domino, _ )) as play) =
    let
        currentDirection =
            domino |> Domino.getDirection

        direction =
            if domino |> Domino.isDouble then
                currentDirection |> Direction.turn

            else
                currentDirection
    in
    play |> mapDomino (Domino.setDirection direction)


setDirection : Direction -> Play -> Play
setDirection direction (Play ( playerName, domino, end )) =
    Play ( playerName, domino |> Domino.setDirection direction, end )


mapDomino : (Domino -> Domino) -> Play -> Play
mapDomino f (Play ( playerName, domino, end )) =
    Play ( playerName, f domino, end )


decoder : Player Name -> Decoder Play
decoder playerName =
    map2
        (\domino end -> Play ( playerName, domino, end ))
        (field "domino" Domino.decoder)
        (oneOf
            [ field "direction" (string |> andThen End.decoder)
            , succeed Left
            ]
        )


encode : GameId -> Play -> Encode.Value
encode gameId (Play ( playerName, domino, end )) =
    Encode.object
        [ ( "gameId", GameId.encode gameId )
        , ( "domino", domino |> Domino.encode )
        , ( "playerName", playerName |> Player.encode )
        , ( "direction", end |> End.encode )
        ]


isForPlayer : Player Name -> Play -> Bool
isForPlayer playerName (Play ( playPlayerName, _, _ )) =
    playerName == playPlayerName


matches : Play -> Play -> Bool
matches (Play ( p1, d1, e2 )) (Play ( p2, d2, e1 )) =
    Domino.matches d1 d2 && e1 == e2 && p1 == p2


view : Play -> Html (Msg Domino)
view (Play ( _, domino, _ )) =
    domino |> Domino.view


toString : Play -> String
toString (Play ( playerName, domino, end )) =
    "Player: "
        ++ Player.toName playerName
        ++ ", Domino: "
        ++ Domino.toString domino
        ++ ", End: "
        ++ End.toString end


errorToString : Error -> String
errorToString error =
    case error of
        Invalid domino end suit ->
            "Cannot play "
                ++ Domino.toString domino
                ++ " at end "
                ++ End.toString end
                ++ " with suit "
                ++ Suit.toString suit
