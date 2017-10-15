module Views.Page exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Data.User as User exposing (User)
import Data.Card as Card exposing (Card)
import Html.CssHelpers
import Styling.MyCss as Css


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"


type ActivePage
    = Home
    | Login


frame : Bool -> Maybe User -> ActivePage -> Html msg -> Html msg
frame isLoading user page content =
    div [] []


menuOption : String -> List (Html.Attribute msg) -> Html msg
menuOption value attrs =
    div [ class [ Css.MenuOption ] ] [ a attrs [ text value ] ]


flashCard : Card -> List (Html.Attribute msg) -> Html msg
flashCard card attrs =
    div [ class [ Css.FlashCardContainer ] ] [ p attrs [ text card.contentEn ] ]


timerField : String -> List (Html.Attribute msg) -> Html msg
timerField value attrs =
    div [ class [ Css.TimerContainer ] ] [ p attrs [ text value ] ]


rowContainer : List (Html msg) -> Html msg
rowContainer html =
    div [ class [ Css.RowContainer ] ] html


flashCardBtn : String -> List (Html.Attribute msg) -> Html msg
flashCardBtn value attrs =
    div [ class [ Css.FlashCardBtn ] ] [ a attrs [ text value ] ]


arrowRightBtn : Html msg
arrowRightBtn =
    div [ class [ Css.ArrowContainer ] ]
        [ div [ class [ Css.ArrowRight ] ] [] ]


arrowLeftBtn : Html msg
arrowLeftBtn =
    div [ class [ Css.ArrowContainer ] ]
        [ div [ class [ Css.ArrowLeft ] ] [] ]



--
-- arrowLeftBtn : Html msg
-- arrowLeftBtn =
