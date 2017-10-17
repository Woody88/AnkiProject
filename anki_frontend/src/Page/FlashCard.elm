module Page.FlashCard exposing (..)

import Data.Card as Card exposing (Card)
import Data.AnkiCard as AnkiCard exposing (AnkiCard, getAnkis)
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Task exposing (Task)
import Route exposing (Route)
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Html.CssHelpers
import Styling.MyCss as Css
import Views.Page as Page
import List
import Debug


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"


type alias Model =
    { cards : List AnkiCard
    }


type Msg
    = NoOp


initialModel : Model
initialModel =
    { cards = []
    }


init : Session -> Task PageLoadError Model
init session =
    let
        loadAnkis =
            AnkiCard.getAnkis
                |> Http.toTask

        handleLoadError _ =
            pageLoadError Page.FlashCard "FlashCard is currently unavailable."
    in
        Task.map Model loadAnkis
            |> Task.mapError handleLoadError


view : Session -> Model -> Html Msg
view session model =
    let
        d =
            Debug.log "check: " model
    in
        div [ class [ Css.MenuContainer ] ]
            [ Page.arrowLeftBtn
            , div [ class [ Css.MenuContainerCol ] ]
                [ Page.flashCard (List.head model.cards) []
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
