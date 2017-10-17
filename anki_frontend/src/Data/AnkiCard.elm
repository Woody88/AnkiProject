module Data.AnkiCard exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias AnkiCard =
    { contentEn : String
    , contentJpKanji : String
    , contentJp : String
    , contextEn : String
    , contextJP : String
    , property : String
    }


decodeAnkiCard : Decoder AnkiCard
decodeAnkiCard =
    decode AnkiCard
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
                [ "http://localhost:3000"
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
