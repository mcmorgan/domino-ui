module Game.Player.DragCoordinates exposing
    ( DragCoordinates
    , decoder
    )

import Json.Decode
    exposing
        ( Decoder
        , at
        , int
        , map2
        )


type alias DragCoordinates =
    { pageX : Int
    , pageY : Int
    }


decoder : Decoder DragCoordinates
decoder =
    map2 DragCoordinates
        (at [ "pageX" ] int)
        (at [ "pageY" ] int)
