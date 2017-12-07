module Page.FlashCard exposing (..)

import Data.Card as Card exposing (Card)
import Data.AnkiCard as AnkiCard exposing (AnkiCard, getAnkis, getAnkiByCardId, postAnkis, putAnkiByCardId)
import Data.Session as Session exposing (Session, Token(..))
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
    , successEdit : Maybe (Result Http.Error (Maybe AnkiCard))
    , listView : Bool
    , editView : Bool
    }


type Msg
    = NoOp
    | SubmitNewAnki
    | UpdateAnki
    | SetContentEn String
    | SetContentKanji String
    | SetContentHirag String
    | SetContextEn String
    | SetContextJp String
    | SetProperty String
    | SetSuccessPost (Result Http.Error AnkiCard)
    | SetSuccessEdit (Result Http.Error (Maybe AnkiCard))


initialModel : Model
initialModel =
    { cards = []
    , newCard = Nothing
    , successPostPost = Nothing
    , successEdit = Nothing
    , listView = False
    , editView = False
    }


init : Session -> Bool -> Task PageLoadError Model
init session listview =
    let
        d = Debug.log "session" session
        token = withDefault (Token "") session.token
        loadAnki =
            (AnkiCard.getAnkis token)
                |> Http.toTask

        toModel cardList =
            { initialModel | cards = cardList, listView = listview }

        handleLoadError message _ =
            pageLoadError Page.FlashCard message
    in
        case session.token of
            Nothing ->
                    Task.fail initialModel
                    |> Task.map toModel
                    |> Task.mapError (handleLoadError "Please Sign In.")

            Just token ->
                    loadAnki
                    |> Task.map toModel
                    |> Task.mapError (handleLoadError "FlashCard is currently unavailable.")






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


initCardList : Model
initCardList =
    { initialModel | listView = True }


initEditCard : Session -> Int -> Task PageLoadError Model
initEditCard session cardId =
    let
        d = Debug.log "session" session
        loadAnki =
            (AnkiCard.getAnkiByCardId cardId)
                |> Http.toTask

        toModel card =
            { initialModel | newCard = card, editView = True }

        handleLoadError message _ =
            pageLoadError Page.FlashCard message
    in
        case session.token of
            Nothing ->
                    Task.fail initialModel
                    |> Task.map toModel
                    |> Task.mapError (handleLoadError "Please Sign In.")

            Just token ->
                    loadAnki
                    |> Task.map toModel
                    |> Task.mapError (handleLoadError "FlashCard is currently unavailable.")




initNewCard : Model
initNewCard =
    let
        card =
            emptyCard
    in
        { initialModel | newCard = Just card }


newCardForm : AnkiCard -> Msg -> Html Msg
newCardForm card submiter =
    Html.form [ class [ Css.FormControl ] ]
        [ Form.input [ placeholder "English Anki", onInput SetContentEn, value card.contentEn ]
        , Form.input [ placeholder "Kanji Anki", onInput SetContentKanji, value card.contentJpKanji ]
        , Form.input [ placeholder "Hiragana Anki", onInput SetContentHirag, value card.contentJp ]
        , Form.textarea [ placeholder "Anki English Context", onInput SetContextEn, value card.contextEn ]
        , Form.textarea [ placeholder "Anki Japanese Context", onInput SetContextJp, value card.contextJP ]
        , Form.input [ placeholder "Anki Property", onInput SetProperty, value card.property ]
        , Form.submit [ onClick submiter ] "Add Anki"
        ]


listView : Session -> Model -> Html Msg
listView session model =
    div [ class [ Css.MenuContainer ] ] (cards model.cards)



--cards : List AnkiCard -> List (List (Html Msg))


cards list =
    List.map (\x -> Page.menuOption x.contentEn [ Route.href (Route.EditFlashCard x.cardId) ]) list


view : Session -> Model -> Html Msg
view session model =
    let
        d =
            Debug.log "check: " model
    in
        if (model.listView) then
            listView session model
        else
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
                        (if model.editView then
                            UpdateAnki
                         else
                            SubmitNewAnki
                        )


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

            UpdateAnki ->
                case model.newCard of
                    Just c ->
                        let
                            p =
                                putAnkiByCardId c.cardId c
                                    |> Http.send SetSuccessEdit
                        in
                            model ! [ p ]

                    Nothing ->
                        model ! []

            SetSuccessEdit result ->
                ( { model | successEdit = Just result }
                , Cmd.none
                )

            SetSuccessPost result ->
                ( { model | successPostPost = Just result }
                , Cmd.none
                )
