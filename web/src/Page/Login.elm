module Page.Login exposing (ExternalMsg(..), Model, Msg, initialModel, update, view)

import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (placeholder)
import Html.CssHelpers
import Styling.MyCss as Css
import Views.Form as Form


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"


type alias Model =
    { errors : String
    , email : String
    , password : String
    }


initialModel : Model
initialModel =
    { errors = ""
    , email = ""
    , password = ""
    }


view : Session -> Model -> Html Msg
view session model =
    form


form : Html Msg
form =
    Html.form [ class [ Css.FormControl ] ]
        [ Form.input [ placeholder "Email" ]
        , Form.password [ placeholder "Password" ]
        , Form.submit [] "Login"
        ]


type Msg
    = NoOp


type ExternalMsg
    = No


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        NoOp ->
            ( model ! [], No )
