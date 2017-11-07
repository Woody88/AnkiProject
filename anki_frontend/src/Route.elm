module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string, int)
import Html exposing (Attribute)
import Html.Attributes as Attr


-- ROUTING --


type Route
    = Login
    | Home
    | FlashCard
    | NewFlashCard
    | FlashCardList
    | EditFlashCard Int


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Login (s "login")
        , Url.map FlashCard (s "flashcard")
        , Url.map NewFlashCard (s "newflashcard")
        , Url.map FlashCardList (s "flashcards")
        , Url.map EditFlashCard (s "flashcards" </> Url.int)
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

                FlashCardList ->
                    [ "flashcards" ]

                EditFlashCard id_ ->
                    [ "flashcards/" ++ (toString id_) ]
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
