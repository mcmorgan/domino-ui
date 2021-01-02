module UI.Modal exposing
    ( Modal
    , close
    , create
    , open
    , view
    )

import Html.Styled exposing (Html, div, h2, p, text)
import Html.Styled.Attributes exposing (class)
import UI.Button as Button exposing (Button)


type Modal msg
    = Modal (Model msg)


type alias Model msg =
    { opened : Bool
    , title : String
    , body : Html msg
    , onClose : msg
    , buttons : List (Button msg)
    }


open : Modal msg -> Modal msg
open (Modal modal) =
    Modal { modal | opened = True }


close : Modal msg -> Modal msg
close (Modal modal) =
    Modal { modal | opened = False }


create : { title : String, body : Html msg, onClose : msg, buttons : List (Button msg) } -> Modal msg
create { title, body, onClose, buttons } =
    Modal
        { opened = False
        , title = title
        , body = body
        , onClose = onClose
        , buttons = buttons
        }


view : Modal msg -> Html msg
view (Modal { title, body, onClose, buttons }) =
    div [ class "modal" ]
        [ div [ class "overlay" ] []
        , div [ class "modal_content" ]
            [ h2 [] [ text title ]
            , p []
                [ body
                ]
            , div [ class "buttons_wrapper" ]
                ((buttons
                    |> List.map Button.view
                 )
                    ++ [ div [ class "close_modal" ]
                            [ Button.danger
                                { action = onClose
                                , label = "x"
                                }
                                |> Button.view
                            ]
                       ]
                )
            ]
        ]
