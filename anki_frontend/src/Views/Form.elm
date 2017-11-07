module Views.Form exposing (..)

import Html exposing (Attribute, Html, fieldset, li, textarea, text, ul, div)
import Html.Attributes exposing (class, type_, href, style)
import Html.CssHelpers
import Styling.MyCss as Css


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"


password : List (Attribute msg) -> Html msg
password attrs =
    div [ class [ Css.InputControl ] ]
        [ Html.input ([ type_ "password" ] ++ attrs) [] ]


input : List (Attribute msg) -> Html msg
input attrs =
    div [ class [ Css.InputControl ] ]
        [ Html.input ([ type_ "text" ] ++ attrs) [] ]


textarea : List (Attribute msg) -> Html msg
textarea attrs =
    div [ class [ Css.TextAreaControl ] ]
        [ Html.textarea (attrs) [] ]


submit : List (Attribute msg) -> String -> Html msg
submit attrs text =
    div ([ class [ Css.SubmitControl ], href "#" ] ++ attrs)
        [ Html.text text ]
