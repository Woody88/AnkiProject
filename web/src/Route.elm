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
        [ Url.map Login (s "")
        , Url.map Home (s "home")
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
                    [ "home" ]

                Login ->
                    [ "" ]

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
        Just Login
    else
        parseHash route location


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.newUrl
