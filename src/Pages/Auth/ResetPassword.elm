module Pages.Auth.ResetPassword exposing (Model, Msg, page)

import Bulma.Classes as B
import Css exposing (..)
import Effect exposing (Effect)
import Gen.Params.Auth.ResetPassword exposing (Params)
import Gen.Route
import Html.Styled exposing (a, button, div, form, hr, input, label, p, text)
import Html.Styled.Attributes exposing (class, css, href, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Json.Encode as Json
import Page
import Ports.Auth.ResetPassword as Ports
import Request
import Shared
import Styles
import Svg.Styled.Attributes exposing (in_)
import View exposing (View)



-- VIEW


view : Model -> View Msg
view model =
    { title = "パスワードリセット"
    , body =
        List.map Html.Styled.toUnstyled
            [ form [ css Styles.formSmall ]
                [ label [] [ text "ユーザID" ]
                , input [ value model.input.userId, onInput <| ChangeInput setUserId, class B.input ] []
                , label [] [ text "メールで送信したコード" ]
                , input [ value model.input.code, onInput <| ChangeInput setCode, class B.input ] []
                , label [] [ text "新しいパスワード" ]
                , input [ value model.input.password, onInput <| ChangeInput setPassword, class B.input, type_ "password" ] []
                , hr [] []
                , button [ onClick OnSubmit, class B.button, type_ B.button ] [ text "送信" ]
                ]
            , p [] [ text model.errorMessage ]
            , div [] [ a [ href <| Gen.Route.toHref Gen.Route.Auth__SignIn ] [ text "サインイン画面に戻る" ] ]
            ]
    }


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init req
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Input =
    { userId : String
    , code : String
    , password : String
    }


type alias Model =
    { request : Request.With Params
    , input : Input
    , errorMessage : String
    }


init : Request.With Params -> ( Model, Effect Msg )
init req =
    ( { request = req
      , input = { userId = "", code = "", password = "" }
      , errorMessage = ""
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = ChangeInput (String -> Input -> Input) String
    | OnSubmit
    | SucceedResetPassword Json.Value
    | FailResetPassword Shared.AuthError


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
            ( model, Effect.fromCmd <| Ports.resetPassword model.input )

        FailResetPassword err ->
            case err.code of
                "Confirmation code cannot be empty" ->
                    ( { model | errorMessage = "全ての欄は入力必須です。" }, Effect.none )

                "Username cannot be empty" ->
                    ( { model | errorMessage = "全ての欄は入力必須です。" }, Effect.none )

                "Password cannot be empty" ->
                    ( { model | errorMessage = "全ての欄は入力必須です。" }, Effect.none )

                "UserNotFoundException" ->
                    ( { model | errorMessage = "ユーザIDがまちがっているようです。" }, Effect.none )

                "InvalidPasswordException" ->
                    ( { model | errorMessage = "パスワードには英小文字、英小文字、数字を含めてください。" }, Effect.none )

                _ ->
                    ( { model | errorMessage = "失敗しました。" }, Effect.none )

        SucceedResetPassword _ ->
            ( model, Effect.fromCmd <| Request.pushRoute Gen.Route.Auth__SignIn model.request )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.succeedResetPassword SucceedResetPassword
        , Ports.failResetPassword FailResetPassword
        ]


setUserId : String -> Input -> Input
setUserId s i =
    { i | userId = s }


setCode : String -> Input -> Input
setCode s i =
    { i | code = s }


setPassword : String -> Input -> Input
setPassword s i =
    { i | password = s }
