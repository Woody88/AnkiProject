module Main exposing (..)

-- import Page.Home as Home

import Page.Login as Login
import Data.Session as Session exposing (Session)
import Route exposing (Route)
import Navigation exposing (..)
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Data.User as User exposing (User)
import Page.NotFound as NotFound
import Page.Login as Login
import Page.Home as Home
import Page.FlashCard as FlashCard
import Debug


type Page
    = Blank
    | Login Login.Model
    | Home Home.Model
    | FlashCard FlashCard.Model
    | NotFound



-- | Home Home.Model


type PageState
    = Loaded Page
    | TransitioningFrom Page


type alias Model =
    { session : Session
    , pageState : PageState
    }


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    let
        d =
            Debug.log "check: " location
    in
        setRoute (Route.fromLocation location)
            { pageState = Loaded initialPage
            , session = { user = decodeUserFromJson val }
            }


decodeUserFromJson : Value -> Maybe User
decodeUserFromJson json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString User.decoder >> Result.toMaybe)


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


initialPage : Page
initialPage =
    Login Login.initialModel


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


type Msg
    = SetRoute (Maybe Route)
    | LoginMsg Login.Msg
    | HomeMsg Home.Msg
    | FlashCardMsg FlashCard.Msg


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Nothing ->
            { model | pageState = Loaded NotFound } ! []

        Just Route.Home ->
            { model | pageState = Loaded (Home Home.initialModel) }
                ! []

        -- |> auth
        Just Route.Login ->
            { model | pageState = Loaded (Login Login.initialModel) } ! []

        Just Route.FlashCard ->
            { model | pageState = Loaded (FlashCard FlashCard.initialModel) } ! []


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
                setRoute route model

            ( HomeMsg subMsg, Home subModel ) ->
                toPage Home HomeMsg (Home.update session) subMsg subModel

            ( FlashCardMsg subMsg, FlashCard subModel ) ->
                toPage FlashCard FlashCardMsg (FlashCard.update session) subMsg subModel

            ( LoginMsg subMsg, Login subModel ) ->
                let
                    ( ( pageModel, cmd ), msgFromPage ) =
                        Login.update subMsg subModel

                    newModel =
                        case msgFromPage of
                            Login.No ->
                                model
                in
                    ( { newModel | pageState = Loaded (Login pageModel) }, Cmd.map LoginMsg cmd )

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
