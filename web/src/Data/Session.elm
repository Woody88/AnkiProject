module Data.Session exposing (Session, UserLogin, Token(..), JWT, decodeJwt, postLogin)

import Data.User as User exposing (User)
import Json.Encode as Encode
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http

type Token = Token String

type alias JWT = { token : String }

type alias Session =
    { user  : Maybe User
    , token : Maybe Token
    }

type alias UserLogin =
    { email    : String
    , password : String
    }

encodeUserLogin : UserLogin -> Value
encodeUserLogin user =
    Encode.object
        [ ( "email", Encode.string user.email )
        , ( "password", Encode.string user.password )
        ]


decodeUserLogin : Decoder UserLogin
decodeUserLogin =
    decode UserLogin
        |> required "email" string
        |> required "password" string

decodeJwt : Decoder JWT
decodeJwt =
    decode JWT
        |> required "token" string

postLogin : UserLogin -> Http.Request (JWT)
postLogin body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "login"
                ]
        , body =
            Http.jsonBody (encodeUserLogin body)
        , expect = Http.expectJson decodeJwt
        , timeout =
            Nothing
        , withCredentials =
            False
        }
