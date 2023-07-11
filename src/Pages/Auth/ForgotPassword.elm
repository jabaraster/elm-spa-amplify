module Pages.Auth.ForgotPassword exposing (Model, Msg, page)

import Bulma.Classes as B
import Css exposing (..)
import Effect exposing (Effect)
import Gen.Params.Auth.ForgotPassword exposing (Params)
import Gen.Route
import Html.Styled exposing (a, div, form, hr, input, label, p, text)
import Html.Styled.Attributes exposing (class, css, href, value)
import Html.Styled.Events exposing (onInput)
import Json.Encode as Json
import Page
import Ports.Auth.ForgotPassword as Ports
import Request
import Shared
import Styles
import UI
import View exposing (View)



-- VIEW


view : Model -> View Msg
view model =
    { title = "パスワードリセット"
    , body =
        List.map Html.Styled.toUnstyled
            [ form [ css Styles.formSmall ]
                [ label [] [ text "ユーザID" ]
                , input
                    [ value model.userId
                    , onInput ChangeUserId
                    , class B.input
                    ]
                    []
                , hr [] []
                , UI.submitter OnSubmit model.loading "コードを送信(コード入力画面に遷移します)"
                ]
            , p [] [ text model.errorMessage ]
            , div [] [ a [ href <| Gen.Route.toHref Gen.Route.Auth__ResetPassword ] [ text "既にコードを持っているならこちらへ" ] ]
            , div [] [ a [ href <| Gen.Route.toHref Gen.Route.Auth__SignIn ] [ text "サインイン画面に戻る" ] ]
            ]
    }


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ req =
    Page.advanced
        { init = init req
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { request : Request.With Params
    , userId : String
    , errorMessage : String
    , loading : Bool
    }


init : Request.With Params -> ( Model, Effect Msg )
init req =
    ( { request = req
      , userId = ""
      , errorMessage = ""
      , loading = False
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = ChangeUserId String
    | OnSubmit
    | SucceedForgotPassword Json.Value
    | FailForgotPassword Shared.AuthError


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ChangeUserId v ->
            ( { model | userId = v }, Effect.none )

        OnSubmit ->
            ( { model | loading = True }, Effect.fromCmd <| Ports.forgotPassword { userId = model.userId } )

        SucceedForgotPassword _ ->
            ( model, Effect.fromCmd <| Request.pushRoute Gen.Route.Auth__ResetPassword model.request )

        FailForgotPassword err ->
            fail { model | loading = False } err


fail : Model -> Shared.AuthError -> ( Model, Effect Msg )
fail model _ =
    ( { model | loading = False, errorMessage = "存在しないユーザIDです。" }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.succeedForgotPassword SucceedForgotPassword
        , Ports.failForgotPassword FailForgotPassword
        ]
