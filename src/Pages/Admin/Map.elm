module Pages.Admin.Map exposing (Model, Msg, page)

import UI
import Effect exposing (Effect)
import Gen.Params.Admin.Map exposing (Params)
import Html.Styled exposing (img)
import Html.Styled.Attributes exposing (src)
import Page
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { sharedModel : Shared.Model
    }


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    ( { sharedModel = shared
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    {
        title = "Admin.Map"
        , body = UI.layout [
            img  [src model.sharedModel.kayoinobaAttributeIconUrls.taisou ][]
        ]
    }
