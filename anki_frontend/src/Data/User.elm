module Data.User exposing (User, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)


type alias User =
    { userId : Int
    , userFirstName : String
    , userLastName : String
    , password : String
    }


decoder : Decoder User
decoder =
    decode User
        |> required "userId" Decode.int
        |> required "userFirstName" Decode.string
        |> required "userLastName" Decode.string
        |> required "userPassword" Decode.string


encode : User -> Value
encode user =
    Encode.object
        [ ( "userId", Encode.int user.userId )
        , ( "userFirstName", Encode.string user.userFirstName )
        , ( "userLastName", Encode.string user.userLastName )
        , ( "password", Encode.string user.password )
        ]
