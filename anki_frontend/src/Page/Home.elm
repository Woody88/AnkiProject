module Page.Home exposing (Model, Msg, initialModel, update, view)

import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (style)
import Views.Page as Page
import Html.CssHelpers
import Styling.MyCss as Css


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"


type alias Model =
    { selected : String }


initialModel : Model
initialModel =
    { selected = "" }


view : Session -> Model -> Html Msg
view session model =
    div [ class [ Css.MenuContainer ], style [ ( "zoom", "1.6" ) ] ]
        [ Page.menuOption "Flash Card" [ Route.href Route.FlashCard ]
        , Page.menuOption "Study Place" []
        , Page.menuOption "New Anki" [ Route.href Route.NewFlashCard ]
        ]



-- div []
--     [ Page
--     -- , a [ Route.href Route.Login ] [ text "login page" ]
--     ]


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        NoOp ->
            model ! []


type Msg
    = NoOp
