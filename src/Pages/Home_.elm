module Pages.Home_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Gen.Params.Home_ exposing (Params)
import Html.Styled exposing (h1, text)
import Page
import Request
import Shared
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.protected.advanced <|
        \user ->
            { init = init
            , update = update
            , view = view
            , subscriptions = subscriptions
            }



-- INIT


type alias Model =
    {}


init : ( Model, Effect Msg )
init =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = OnSignOut


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        OnSignOut ->
            ( model, Effect.fromCmd <| Shared.signOut () )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Home"
    , body = UI.layout OnSignOut [ h1 [] [ text "Home" ] ]
    }
