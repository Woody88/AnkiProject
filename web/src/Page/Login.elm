module Page.Login exposing (ExternalMsg(..), Model, Msg, initialModel, update, view)

import Data.Session as Session exposing (Session, UserLogin, JWT, Token(..), postLogin)
import Data.User as User exposing (User)
import Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (..)
import Html.CssHelpers
import Http
import Styling.MyCss as Css
import Views.Form as Form
import Util exposing ((=>))


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"

type Msg
    = Login
    | SetEmail String
    | SetPassword String
    | SetSuccessPost (Result Http.Error JWT)

type ExternalMsg
    = NoOp
    | SetToken Token

type alias Model =
    { errors : String
    , email : String
    , password : String
    , successPostPost : Maybe (Result Http.Error JWT)
    }


initialModel : Model
initialModel =
    { errors = ""
    , email = ""
    , password = ""
    , successPostPost = Nothing
    }


view : Session -> Model -> Html Msg
view session model =
    form


form : Html Msg
form =
    Html.form [ class [ Css.FormControl ] ]
        [ Form.input [ onInput SetEmail, placeholder "Email" ]
        , Form.password [ onInput SetPassword, placeholder "Password" ]
        , Form.submit [onClick Login] "Login"
        ]

update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        SetPassword password ->
            { model | password = password }
                => Cmd.none
                => NoOp

        SetEmail email ->
            { model | email = email }
                => Cmd.none
                => NoOp
        Login ->
            let
                p =
                    postLogin { email = model.email,  password = model.password }
                        |> Http.send SetSuccessPost
            in
                model
                => p
                => NoOp

        SetSuccessPost (Ok jwt) ->
            let
                d =  Debug.log "user: " jwt
            in
               model
               => Route.modifyUrl Route.Home
               => SetToken (Token jwt.token)

        SetSuccessPost (Err result) ->
            let
                d =  Debug.log "user: " result
            in
               model
               => Cmd.none
               => NoOp
