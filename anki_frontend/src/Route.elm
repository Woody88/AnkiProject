module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)
import Html exposing (Attribute)
import Html.Attributes as Attr


-- ROUTING --


type Route
    = Login
    | Home
    | FlashCard
    | NewFlashCard


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Login (s "login")
        , Url.map FlashCard (s "flashcard")
        , Url.map NewFlashCard (s "newflashcard")
        ]


routeToString : Route -> String
routeToString page =
    let
        extras =
            case page of
                Home ->
                    [ "" ]

                Login ->
                    [ "login" ]

                FlashCard ->
                    [ "flashcard" ]

                NewFlashCard ->
                    [ "newflashcard" ]
    in
        "#" ++ String.join "/" extras


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


fromLocation : Location -> Maybe Route
fromLocation location =
    if (String.isEmpty location.hash) then
        Just Home
    else
        parseHash route location


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.newUrl
