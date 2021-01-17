module Main exposing (main)

import Browser
import Css
    exposing
        ( backgroundColor
        , display
        , hex
        , inlineBlock
        , marginBottom
        , padding
        , px
        )
import Game exposing (Game)
import Game.Board as Board exposing (Board)
import Game.Direction as Direction
import Game.Domino as Domino exposing (Domino)
import Game.Domino.Play as Play exposing (Play)
import Game.Domino.Suit exposing (Suit(..))
import Game.End exposing (End(..))
import Game.Hand as Hand exposing (Hand)
import Game.Orientation exposing (Orientation(..))
import Game.Sprite as Sprite exposing (Sprite)
import Html.Styled exposing (Html, div, h1, h2, p, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Player exposing (Msg)


main : Program Flags (Result String Model) (Msg Domino)
main =
    Browser.element { init = init, update = update, view = view >> toUnstyled, subscriptions = subscriptions }


type alias Flags =
    {}


init : Flags -> ( Result String Model, Cmd (Msg Domino) )
init _ =
    let
        model =
            buildBoards
                |> Result.map
                    (\boards ->
                        { sprites = buildSprites
                        , boards = boards
                        , hands = buildHands
                        , games = buildGames
                        }
                    )
    in
    ( model, Cmd.none )



-- UPDATE


update : Msg Domino -> Result String Model -> ( Result String Model, Cmd (Msg Domino) )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )


subscriptions : Result String Model -> Sub msg
subscriptions _ =
    Sub.none


type alias Model =
    { sprites : List Sprite
    , boards : List Board
    , hands : List Hand
    , games : List Game
    }


buildBoards : Result String (List Board)
buildBoards =
    [ buildPlays |> Board.create { rows = 16, columns = 50 }
    , buildPlays |> Board.create { rows = 40, columns = 40 }
    , buildPlays |> Board.create { rows = 50, columns = 16 }
    ]
        |> List.foldr
            (\b boards ->
                case b of
                    Ok board ->
                        Result.map (\list -> board :: list) boards

                    Err err ->
                        Err (err |> Board.errorToString)
            )
            (Ok [])


buildHands : List Hand
buildHands =
    [ Hand.create Direction.South
        |> Hand.addExposed (Domino.create Five Six)
        |> Hand.addExposed (Domino.create Deuce Six)
        |> Hand.addExposed (Domino.create Blank Six)
    , Hand.create Direction.South
        |> Hand.addUnexposed
        |> Hand.addUnexposed
        |> Hand.addUnexposed
        |> Hand.addUnexposed
    ]


buildGames : List Game
buildGames =
    [ Game.create "test" { width = 400, height = 400 } ]


buildSprites : List Sprite
buildSprites =
    Direction.all |> List.map Sprite.create


buildPlays : List Play
buildPlays =
    let
        buildPlay left right end =
            Play.create (Player.current (Just "a")) (Domino.create left right) end
    in
    [ buildPlay Four Four Left
    , buildPlay Deuce Four Left
    , buildPlay Blank Deuce Left
    , buildPlay Blank Blank Left
    , buildPlay Six Blank Left
    , buildPlay Six Six Left
    , buildPlay Six Five Left
    , buildPlay Five Blank Left
    , buildPlay Blank Trey Left
    , buildPlay Trey Trey Left
    , buildPlay Trey Six Left
    , buildPlay Six Deuce Left
    , buildPlay Deuce Deuce Left
    , buildPlay Four Blank Right
    , buildPlay Blank Ace Right
    , buildPlay Ace Ace Right
    , buildPlay Ace Six Right
    , buildPlay Six Four Right
    , buildPlay Four Trey Right
    , buildPlay Ace Trey Right
    , buildPlay Ace Deuce Right
    , buildPlay Five Deuce Right
    ]


view : Result String Model -> Html (Msg Domino)
view resultModel =
    let
        boardView board =
            div []
                [ div
                    [ css
                        [ display inlineBlock
                        , backgroundColor (hex "55af6a")
                        , marginBottom (px 5)
                        ]
                    ]
                    [ Board.view board ]
                ]
    in
    case resultModel of
        Ok { sprites, hands, boards, games } ->
            div
                [ css
                    [ backgroundColor (hex "e9efe9")
                    , padding (px 10)
                    ]
                ]
                [ h1 [] [ text "Demo" ]
                , p
                    []
                    [ h2 [] [ text "Games" ]
                    , div [] <| List.map Game.view2 games
                    , h2 [] [ text "Sprites" ]
                    , p [] <| List.map Sprite.view sprites
                    , h2 [] [ text "Hands" ]
                    , p [] <| List.map Hand.view hands
                    , h2 [] [ text "Boards" ]
                    , div [] <| List.map boardView boards
                    ]
                ]

        Err err ->
            text <| "something went wrong: " ++ err
