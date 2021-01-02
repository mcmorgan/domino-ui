module Game.Layout exposing (Layout(..), create)

import Browser.Dom exposing (Viewport)


type Layout
    = Portrait
    | Landscape
    | Square


create : Viewport -> Layout
create { viewport } =
    let
        viewportRatio =
            viewport.width / viewport.height
    in
    if viewportRatio > 0.8 && viewportRatio < 1.2 then
        Square

    else if viewport.width > viewport.height then
        Landscape

    else
        Portrait
