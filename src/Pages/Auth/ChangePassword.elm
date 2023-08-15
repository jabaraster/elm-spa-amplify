module Pages.Auth.ChangePassword exposing (Model, Msg, page)

import Bulma.Classes as B
import Css exposing (..)
import Domain
import Effect exposing (Effect)
import Gen.Params.Auth.ChangePassword exposing (Params)
import Gen.Route
import Html.Styled exposing (a, button, div, form, hr, input, label, p, text)
import Html.Styled.Attributes exposing (class, css, href, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Page
import Ports.Auth.ChangePassword as Ports
import Request
import Shared
import View exposing (View)
import Views



-- VIEW


view : Model -> View Msg
view model =
    { title = "パスワードの変更"
    , body =
        Views.signInLayout
            [ label [] [ text "現在のパスワード" ]
            , input
                [ value model.input.currentPassword
                , onInput <| ChangeInput setCurrentPassword
                , class B.input
                , type_ "password"
                ]
                []
            , label [] [ text "新しいパスワード" ]
            , input
                [ value model.input.newPassword
                , onInput <| ChangeInput setNewPassword
                , class B.input
                , type_ "password"
                ]
                []
            , label [] [ text "新しいパスワードの確認" ]
            , input
                [ value model.input.confirmedPassword
                , onInput <| ChangeInput setConfirmedPassword
                , class B.input
                , type_ "password"
                ]
                []
            , hr [] []
            , button [ onClick OnSubmit, class B.button, type_ B.button ] [ text "送信" ]
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
    , input : Input
    , errorMessage : String
    }


init : Request.With Params -> ( Model, Effect Msg )
init req =
    ( { request = req
      , input =
            { currentPassword = ""
            , newPassword = ""
            , confirmedPassword = ""
            }
      , errorMessage = ""
      }
    , Effect.none
    )


type alias Input =
    { currentPassword : String, newPassword : String, confirmedPassword : String }



-- UPDATE


type Msg
    = ChangeInput (String -> Input -> Input) String
    | OnSubmit
    | SucceedChangePassword Domain.SignInUser
    | FailChangePassword Shared.AuthError


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ChangeInput ope v ->
            let
                input =
                    model.input
            in
            ( { model | input = ope v input }, Effect.none )

        OnSubmit ->
            if model.input.newPassword == model.input.confirmedPassword then
                ( model, Effect.fromCmd <| Ports.changePassword { newPassword = model.input.newPassword } )

            else
                ( { model | errorMessage = "パスワードが一致しません。" }, Effect.none )

        SucceedChangePassword user ->
            ( model, Effect.fromShared <| Shared.SucceedSignIn user )

        FailChangePassword err ->
            case err.code of
                "InvalidPasswordException" ->
                    ( { model | errorMessage = "パスワードの要件を満たしません。" }, Effect.none )

                "Password cannot be empty" ->
                    ( { model | errorMessage = "全てのパスワード欄を入力してください。" }, Effect.none )

                _ ->
                    ( { model | errorMessage = "パスワード更新に失敗しました。" }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.succeedChangePassword SucceedChangePassword
        , Ports.failChangePassword FailChangePassword
        ]


setCurrentPassword : String -> Input -> Input
setCurrentPassword s i =
    { i | currentPassword = s }


setNewPassword : String -> Input -> Input
setNewPassword s i =
    { i | newPassword = s }


setConfirmedPassword : String -> Input -> Input
setConfirmedPassword s i =
    { i | confirmedPassword = s }
