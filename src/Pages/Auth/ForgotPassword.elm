module Pages.Auth.ForgotPassword exposing (Model, Msg, page)

import Bulma.Classes as B
import Css exposing (..)
import Effect exposing (Effect)
import Gen.Params.Auth.ForgotPassword exposing (Params)
import Gen.Route
import Html.Styled exposing (a, button, div, form, hr, input, label, p, text)
import Html.Styled.Attributes exposing (class, css, href, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Json.Encode as Json
import Page
import Ports.Auth.ForgotPassword as Ports
import Request
import Shared
import Styles
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
                , button [ type_ B.button, class B.button, onClick OnSubmit ] [ text "送信" ]
                ]
            , p [] [ text model.errorMessage ]
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
    }


init : Request.With Params -> ( Model, Effect Msg )
init req =
    ( { request = req
      , userId = ""
      , errorMessage = ""
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = ChangeUserId String
    | OnSubmit
    | SucceedForgotPassword Json.Value
    | FailForgotPassword Json.Value


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ChangeUserId v ->
            ( { model | userId = v }, Effect.none )

        OnSubmit ->
            ( model, Effect.fromCmd <| Ports.forgotPassword { userId = model.userId } )

        SucceedForgotPassword _ ->
            ( model, Effect.fromCmd <| Request.pushRoute Gen.Route.Auth__ResetPassword model.request )

        FailForgotPassword _ ->
            ( { model | errorMessage = "存在しないユーザIDです。" }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.succeedForgotPassword SucceedForgotPassword
        , Ports.failForgotPassword FailForgotPassword
        ]
