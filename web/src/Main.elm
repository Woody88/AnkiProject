module Main exposing (..)

-- import Page.Home as Home

import Page.Login as Login
import Data.Session as Session exposing (Session, Token(..))
import Route exposing (Route)
import Navigation exposing (..)
import Task
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Data.User as User exposing (User)
import Page.NotFound as NotFound
import Page.Errored as Errored exposing (PageLoadError)
import Page.Login as Login
import Page.Home as Home
import Page.FlashCard as FlashCard
import Views.Page as Page exposing (ActivePage)
import Util exposing ((=>))
import Debug


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


type Page
    = Blank
    | Login Login.Model
    | Home Home.Model
    | FlashCard FlashCard.Model
    | NotFound
    | Errored PageLoadError



-- | Home Home.Model


type PageState
    = Loaded Page
    | TransitioningFrom Page


type alias Model =
    { session : Session
    , pageState : PageState
    }

type Msg
    = SetRoute (Maybe Route)
    | LoginMsg Login.Msg
    | HomeMsg Home.Msg
    | FlashCardMsg FlashCard.Msg
    | FlashCardLoaded (Result PageLoadError FlashCard.Model)

initialPage : Page
initialPage =
    Login Login.initialModel

init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    let
        d =
            Debug.log "check: " location
    in
        setRoute (Route.fromLocation location)
            { pageState = Loaded initialPage
            , session = { user = decodeUserFromJson val, token = Nothing }
            }


decodeUserFromJson : Value -> Maybe User
decodeUserFromJson json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString User.decodeUser >> Result.toMaybe)


view : Model -> Html Msg
view model =
    let
        d =
            Debug.log "page state: " model.pageState
    in
        case model.pageState of
            Loaded page ->
                viewPage model.session False page

            TransitioningFrom page ->
                viewPage model.session True page


viewPage : Session -> Bool -> Page -> Html Msg
viewPage session isLoading page =
    case page of
        NotFound ->
            NotFound.view session

        Blank ->
            -- This is for the very initial page load, while we are loading
            -- data via HTTP. We could also render a spinner here.
            Html.text "blank"

        Login subModel ->
            Login.view session subModel
                |> Html.map LoginMsg

        Home subModel ->
            Home.view session subModel
                |> Html.map HomeMsg

        FlashCard subModel ->
            FlashCard.view session subModel
                |> Html.map FlashCardMsg

        Errored subModel ->
            Errored.view session subModel

getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none





pageErrored : Model -> ActivePage -> String -> ( Model, Cmd msg )
pageErrored model activePage errorMessage =
    let
        error =
            Errored.pageLoadError activePage errorMessage
    in
        { model | pageState = Loaded (Errored error) } => Cmd.none


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        transition toMsg task =
            { model | pageState = TransitioningFrom (getPage model.pageState) }
                => Task.attempt toMsg task

        errored =
            pageErrored model

        maybeToken = model.session.token
    in
        case maybeToken of
            Nothing ->
                { model | pageState = Loaded (Login Login.initialModel) } ! []
            _ -> setRoute_ maybeRoute model transition errored

setRoute_ maybeRoute model transition errored =
    case maybeRoute of
        Nothing ->
            { model | pageState = Loaded NotFound } ! []

        Just Route.Home ->
            { model | pageState = Loaded (Home Home.initialModel) }
                ! []

        -- |> auth
        Just Route.Login ->
            { model | pageState = Loaded (Home Home.initialModel) }
                ! []
            --{ model | pageState = Loaded (Login Login.initialModel) } ! []

        Just Route.NewFlashCard ->
            { model | pageState = Loaded (FlashCard FlashCard.initNewCard) } ! []

        Just (Route.EditFlashCard id_) ->
            transition FlashCardLoaded (FlashCard.initEditCard model.session id_)

        Just Route.FlashCardList ->
            transition FlashCardLoaded (FlashCard.init model.session True)

        Just Route.FlashCard ->
            transition FlashCardLoaded (FlashCard.init model.session False)


auth : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
auth ( model, cmd ) =
    case model.session.user of
        Nothing ->
            setRoute (Just Route.Login) model

        _ ->
            ( model, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        d =
            Debug.log "in update page"

        session =
            model.session

        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
                ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )
    in
        case ( msg, page ) of
            ( SetRoute route, _ ) ->
                let
                    d = Debug.log "Setting Route"
                in setRoute route model

            ( FlashCardLoaded (Ok subModel), _ ) ->
                { model | pageState = Loaded (FlashCard subModel) } => Cmd.none

            ( FlashCardLoaded (Err error), _ ) ->
                { model | pageState = Loaded (Errored error) } => Cmd.none

            ( HomeMsg subMsg, Home subModel ) ->
                toPage Home HomeMsg (Home.update session) subMsg subModel

            ( FlashCardMsg subMsg, FlashCard subModel ) ->
                toPage FlashCard FlashCardMsg (FlashCard.update session) subMsg subModel
            --
            -- ( LoginMsg subMsg, Login subModel ) ->
            --     let
            --         ( pageModel, cmd ) =
            --             Login.update subMsg subModel
            --     in
            --         ( { model | pageState = Loaded (Login pageModel) }, Cmd.map LoginMsg cmd )
            ( LoginMsg subMsg, Login subModel ) ->
                let
                    ( ( pageModel, cmd ), msgFromPage ) =
                        Login.update subMsg subModel

                    newModel =
                        case msgFromPage of
                            Login.NoOp ->
                                model

                            Login.SetToken token ->
                                let
                                    session =
                                        model.session
                                in
                                { model | session = { user = Nothing, token = Just token } }
                in
                    { newModel | pageState = Loaded (Login pageModel) }
                    => Cmd.map LoginMsg cmd

            ( _, NotFound ) ->
                -- Disregard incoming messages when we're on the
                -- NotFound page.
                model ! []

            ( _, _ ) ->
                -- Disregard incoming messages that arrived for the wrong page
                model ! []


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
