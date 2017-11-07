module Data.User exposing (..)

import Json.Encode as Encode
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http
import String


type alias User =
    { userId : Int
    , userFirstName : String
    , userLastName : String
    , userPassword : String
    }


encodeUser : User -> Value
encodeUser user =
    Encode.object
        [ ( "userId", Encode.int user.userId )
        , ( "userFirstName", Encode.string user.userFirstName )
        , ( "userLastName", Encode.string user.userLastName )
        , ( "password", Encode.string user.userPassword )
        ]


decodeUser : Decoder User
decodeUser =
    decode User
        |> required "userId" int
        |> required "userFirstName" string
        |> required "userLastName" string
        |> required "userPassword" string


getUsers : Http.Request (List User)
getUsers =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "users"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeUser)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postUser : User -> Http.Request User
postUser body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "user"
                ]
        , body =
            Http.jsonBody (encodeUser body)
        , expect =
            Http.expectJson decodeUser
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getUserById : Int -> Http.Request (Maybe User)
getUserById capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "user"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (maybe decodeUser)
        , timeout =
            Nothing
        , withCredentials =
            False
        }
