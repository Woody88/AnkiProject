module Page.FlashCard exposing (..)

import Data.Card as Card exposing (Card)
import Data.AnkiCard as AnkiCard exposing (AnkiCard, getAnkis, postAnkis)
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task exposing (Task)
import Route exposing (Route)
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Html.CssHelpers
import Styling.MyCss as Css
import Views.Page as Page
import Views.Form as Form
import List
import Debug
import Maybe exposing (withDefault)


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"


type alias Model =
    { cards : List AnkiCard
    , newCard : Maybe AnkiCard
    , successPostPost : Maybe (Result Http.Error AnkiCard)
    }


type Msg
    = NoOp
    | SubmitNewAnki
    | SetContentEn String
    | SetContentKanji String
    | SetContentHirag String
    | SetContextEn String
    | SetContextJp String
    | SetProperty String
    | SetSuccessPost (Result Http.Error AnkiCard)


initialModel : Model
initialModel =
    { cards = []
    , newCard = Nothing
    , successPostPost = Nothing
    }


init : Session -> Task PageLoadError Model
init session =
    let
        loadAnkis =
            (AnkiCard.getAnkis)
                |> Http.toTask

        toModel cardList =
            { cards = cardList
            , newCard = Nothing
            , successPostPost = Nothing
            }

        handleLoadError _ =
            pageLoadError Page.FlashCard "FlashCard is currently unavailable."
    in
        loadAnkis
            |> Task.map toModel
            |> Task.mapError handleLoadError


emptyCard : AnkiCard
emptyCard =
    { cardId = 0
    , contentEn = ""
    , contentJpKanji = ""
    , contentJp = ""
    , contextEn = ""
    , contextJP = ""
    , property = ""
    }


initNewCard : Model
initNewCard =
    let
        card =
            emptyCard
    in
        { initialModel | newCard = Just card }


newCardForm : AnkiCard -> Html Msg
newCardForm card =
    Html.form [ class [ Css.FormControl ] ]
        [ Form.input [ placeholder "English Anki", onInput SetContentEn ]
        , Form.input [ placeholder "Kanji Anki", onInput SetContentKanji ]
        , Form.input [ placeholder "Hiragana Anki", onInput SetContentHirag ]
        , Form.textarea [ placeholder "Anki English Context", onInput SetContextEn ]
        , Form.textarea [ placeholder "Anki Japanese Context", onInput SetContextJp ]
        , Form.input [ placeholder "Anki Property", onInput SetProperty ]
        , Form.submit [ onClick SubmitNewAnki ] "Add Anki"
        ]


view : Session -> Model -> Html Msg
view session model =
    let
        d =
            Debug.log "check: " model
    in
        case model.newCard of
            Nothing ->
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

            Just card ->
                newCardForm card


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    let
        card =
            case model.newCard of
                Just c ->
                    c

                _ ->
                    emptyCard
    in
        case msg of
            NoOp ->
                model ! []

            SetContentEn v ->
                let
                    c =
                        { card | contentEn = v }

                    newModel =
                        { model | newCard = Just c }
                in
                    newModel ! []

            SetContentKanji v ->
                let
                    c =
                        { card | contentJpKanji = v }

                    newModel =
                        { model | newCard = Just c }
                in
                    newModel ! []

            SetContentHirag v ->
                let
                    c =
                        { card | contentJp = v }

                    newModel =
                        { model | newCard = Just c }
                in
                    newModel ! []

            SetContextEn v ->
                let
                    c =
                        { card | contextEn = v }

                    newModel =
                        { model | newCard = Just c }
                in
                    newModel ! []

            SetContextJp v ->
                let
                    c =
                        { card | contextJP = v }

                    newModel =
                        { model | newCard = Just c }
                in
                    newModel ! []

            SetProperty v ->
                let
                    c =
                        { card | property = v }

                    newModel =
                        { model | newCard = Just c }
                in
                    newModel ! []

            SubmitNewAnki ->
                let
                    p =
                        postAnkis (withDefault emptyCard model.newCard)
                            |> Http.send SetSuccessPost
                in
                    model ! [ p ]

            SetSuccessPost result ->
                ( { model | successPostPost = Just result }
                , Cmd.none
                )
