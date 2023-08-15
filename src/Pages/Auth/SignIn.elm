module Pages.Auth.SignIn exposing (Model, Msg, page)

import Bulma.Classes as B
import Css exposing (..)
import Domain
import Effect exposing (Effect)
import Gen.Params.Auth.SignIn exposing (Params)
import Gen.Route
import Html.Styled exposing (a, button, div, form, hr, input, label, p, text)
import Html.Styled.Attributes exposing (class, css, href, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Json.Encode as Json
import Page
import Ports.Auth.SignIn as Ports
import Request
import Shared
import Styles
import View exposing (View)
import Views exposing (submitter)


view : Model -> View Msg
view model =
    { title = "サインイン"
    , body =
        Views.signInLayout
            [ label [] [ text "ユーザID" ]
            , input
                [ value model.input.userId
                , onInput <| ChangeInput setUserId
                , class B.input
                ]
                []
            , label [] [ text "パスワード" ]
            , input
                [ value model.input.password
                , class B.input
                , onInput <| ChangeInput setPassword
                , type_ "password"
                ]
                []
            , a [ href <| Gen.Route.toHref Gen.Route.Auth__ForgotPassword ] [ text "パスワードを忘れた場合はこちら" ]
            , hr [] []
            , submitter OnSubmit model.loading "サインイン"
            , p [] [ text model.errorMessage ]
            , hr [] []
            , p [] [ text "新しくアカウントが必要な場合はサムライト社担当にご連絡をお願いします。" ]
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


type alias Input =
    { userId : String
    , password : String
    }


type alias Model =
    { request : Request.With Params
    , input : Input
    , errorMessage : String
    , loading : Bool
    }


init : Request.With Params -> ( Model, Effect Msg )
init req =
    ( { request = req
      , input =
            { userId = ""
            , password = ""
            }
      , errorMessage = ""
      , loading = False
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = ChangeInput (String -> Input -> Input) String
    | OnSubmit
    | SucceedSignIn Domain.SignInUser
    | RequireChangePassword Json.Value
    | FailSignIn Shared.AuthError


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
            ( { model | loading = True }
            , Effect.fromCmd <| Ports.signIn { userId = model.input.userId, password = model.input.password }
            )

        SucceedSignIn user ->
            ( model, Effect.fromShared <| Shared.SucceedSignIn user )

        RequireChangePassword _ ->
            ( model, Effect.fromCmd <| Request.pushRoute Gen.Route.Auth__ChangePassword model.request )

        FailSignIn err ->
            fail { model | loading = False } err


fail : Model -> Shared.AuthError -> ( Model, Effect Msg )
fail model err =
    case err.code of
        "NotAuthorizedException" ->
            ( { model | errorMessage = "ユーザID、パスワードが違います。" }, Effect.none )

        "UserNotFoundException" ->
            ( { model | errorMessage = "ユーザID、パスワードが違います。" }, Effect.none )

        "InvalidParameterException" ->
            ( { model | errorMessage = "パスワードを入力してください。" }, Effect.none )

        "Username cannot be empty" ->
            ( { model | errorMessage = "ユーザIDを入力してください。" }, Effect.none )

        _ ->
            ( { model | errorMessage = "ログインに失敗しました。code[" ++ err.code ++ "] message[" ++ err.message ++ "]" }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.succeedSignIn SucceedSignIn
        , Ports.failSignIn FailSignIn
        , Ports.requireChangePassword RequireChangePassword
        ]


setUserId : String -> Input -> Input
setUserId s i =
    { i | userId = s }


setPassword : String -> Input -> Input
setPassword s i =
    { i | password = s }
