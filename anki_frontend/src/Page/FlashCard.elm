module Page.FlashCard exposing (..)

import Data.Card as Card exposing (Card)
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)
import Html.CssHelpers
import Styling.MyCss as Css
import Views.Page as Page


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"


type alias Model =
    { cards : List Card
    , currentCard : Card
    , previousCard : Card
    , nextCard : Card
    }


type Msg
    = NoOp


initialModel : Model
initialModel =
    { cards = []
    , currentCard = Card.new
    , previousCard = Card.new
    , nextCard = Card.new
    }


view : Session -> Model -> Html Msg
view session model =
    div [ class [ Css.MenuContainer ] ]
        [ Page.arrowLeftBtn
        , div [ class [ Css.MenuContainerCol ] ]
            [ Page.flashCard model.currentCard []
            , Page.timerField "0:30" []
            , Page.rowContainer
                [ Page.flashCardBtn "Option 1" []
                , Page.flashCardBtn "Option 2" []
                , Page.flashCardBtn "Option 3" []
                , Page.flashCardBtn "Option 4" []
                ]
            ]
        , Page.arrowRightBtn
        ]


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        NoOp ->
            model ! []
