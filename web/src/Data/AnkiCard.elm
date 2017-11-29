module Data.AnkiCard exposing (..)

import Json.Encode as Encode
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
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


getAnkis : Http.Request (List AnkiCard)
getAnkis =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "ankis"
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


postAnkis : AnkiCard -> Http.Request AnkiCard
postAnkis body =
    Http.request
        { method =
            "POST"
        , headers =
            []
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


getAnkiByCardId : Int -> Http.Request (Maybe AnkiCard)
getAnkiByCardId capture_cardId =
    Http.request
        { method =
            "GET"
        , headers =
            []
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


putAnkiByCardId : Int -> AnkiCard -> Http.Request (Maybe AnkiCard)
putAnkiByCardId capture_cardId body =
    Http.request
        { method =
            "PUT"
        , headers =
            []
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
