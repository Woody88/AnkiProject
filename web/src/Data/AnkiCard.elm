module Data.AnkiCard exposing (..)

import Json.Encode as Encode
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Data.Session exposing (Token(..))
import Http
import String


type alias AnkiCard =
    { cardId : Int
    , contentEn : String
    , contentJpKanji : String
    , contentJp : String
    , contextEn : String
    , contextJP : String
    , property : String
    }


encodeAnkiCard : AnkiCard -> Value
encodeAnkiCard card =
    Encode.object
        [ ( "cardId", Encode.int card.cardId )
        , ( "contentEn", Encode.string card.contentEn )
        , ( "contentJpKanji", Encode.string card.contentJpKanji )
        , ( "contentJp", Encode.string card.contentJp )
        , ( "contextEn", Encode.string card.contextEn )
        , ( "contextJP", Encode.string card.contextJP )
        , ( "property", Encode.string card.property )
        ]


decodeAnkiCard : Decoder AnkiCard
decodeAnkiCard =
    decode AnkiCard
        |> required "cardId" int
        |> required "contentEn" string
        |> required "contentJpKanji" string
        |> required "contentJp" string
        |> required "contextEn" string
        |> required "contextJP" string
        |> required "property" string


getAnkis : Token -> Http.Request (List AnkiCard)
getAnkis (Token token)=
    Http.request
        { method =
            "GET"
        , headers =
            [Http.header "Authorization" ("Bearer " ++ token)]
        , url =
            String.join "/"
                [ ""
                , "anki"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeAnkiCard)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postAnkis : Token -> AnkiCard -> Http.Request AnkiCard
postAnkis (Token token) body =
    Http.request
        { method =
            "POST"
        , headers =
            [Http.header "Authorization" ("Bearer " ++ token)]
        , url =
            String.join "/"
                [ ""
                , "ankis"
                ]
        , body =
            Http.jsonBody (encodeAnkiCard body)
        , expect =
            Http.expectJson decodeAnkiCard
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getAnkiByCardId : Token -> Int -> Http.Request (Maybe AnkiCard)
getAnkiByCardId (Token token) capture_cardId =
    Http.request
        { method =
            "GET"
        , headers =
            [Http.header "Authorization" ("Bearer " ++ token)]
        , url =
            String.join "/"
                [ ""
                , "anki"
                , capture_cardId |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (maybe decodeAnkiCard)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


putAnkiByCardId : Token -> Int -> AnkiCard -> Http.Request (Maybe AnkiCard)
putAnkiByCardId (Token token) capture_cardId body =
    Http.request
        { method =
            "PUT"
        , headers =
            [Http.header "Authorization" ("Bearer " ++ token)]
        , url =
            String.join "/"
                [ ""
                , "anki"
                , capture_cardId |> toString |> Http.encodeUri
                ]
        , body =
            Http.jsonBody (encodeAnkiCard body)
        , expect =
            Http.expectJson (maybe decodeAnkiCard)
        , timeout =
            Nothing
        , withCredentials =
            False
        }
