module Pages.Admin.Map exposing (Model, Msg, page)

import Domain
import Effect exposing (Effect)
import Gen.Params.Admin.Map exposing (Params)
import Html.Styled exposing (img)
import Html.Styled.Attributes exposing (src)
import Page
import Request exposing (Request)
import Shared
import UI
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.protected.advanced <|
        \user ->
            { init = init shared
            , update = update
            , view = view user
            , subscriptions = \_ -> Sub.none
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


view : Domain.User -> Model -> View Msg
view user model =
    { title = "Admin.Map"
    , body =
        UI.layout
            [ img [ src model.sharedModel.kayoinobaAttributeIconUrls.taisou ] []
            ]
    }
