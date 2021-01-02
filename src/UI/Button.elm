module UI.Button exposing (Button, cta, danger, primary, secondary, view, warning)

import Html.Styled exposing (Html, button, text)
import Html.Styled.Attributes exposing (classList)
import Html.Styled.Events exposing (onClick)


type Button msg
    = Button
        { type_ : Type
        , action : msg
        , label : String
        }


type Type
    = CTA
    | Primary
    | Secondary
    | Danger
    | Warning


cta : { action : msg, label : String } -> Button msg
cta { action, label } =
    Button { type_ = CTA, action = action, label = label }


primary : { action : msg, label : String } -> Button msg
primary { action, label } =
    Button { type_ = Primary, action = action, label = label }


secondary : { action : msg, label : String } -> Button msg
secondary { action, label } =
    Button { type_ = Secondary, action = action, label = label }


danger : { action : msg, label : String } -> Button msg
danger { action, label } =
    Button { type_ = Danger, action = action, label = label }


warning : { action : msg, label : String } -> Button msg
warning { action, label } =
    Button { type_ = Warning, action = action, label = label }


view : Button msg -> Html msg
view (Button { action, type_, label }) =
    let
        kindClasses =
            case type_ of
                Primary ->
                    [ ( "button-blue", True ) ]

                Secondary ->
                    []

                Danger ->
                    [ ( "button-red", True ) ]

                CTA ->
                    [ ( "button-green", True ) ]

                Warning ->
                    [ ( "button-yellow", True ) ]
    in
    button
        [ classList
            (( "button", True )
                :: kindClasses
            )
        , onClick action
        ]
        [ text <| label ]
