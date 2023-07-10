module Pages.Auth.SignIn exposing (Model, Msg, page)

import Bulma.Classes as B
import Effect exposing (Effect)
import Gen.Params.Auth.SignIn exposing (Params)
import Html.Styled exposing (button, form, input, label, p, text)
import Html.Styled.Attributes exposing (class, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Json.Encode as Json
import Page
import Request
import Shared
import View exposing (View)


view : Model -> View Msg
view model =
    { title = "Sign in"
    , body =
        List.map Html.Styled.toUnstyled
            [ form []
                [ label [] [ text "ユーザID" ]
                , input
                    [ value model.inputUserId
                    , onInput ChangeInputUserId
                    , class B.input
                    ]
                    []
                , label [] [ text "パスワード" ]
                , input
                    [ value model.inputPassword
                    , class B.input
                    , onInput ChangeInputPassword
                    , type_ "password"
                    ]
                    []
                , button [ type_ B.button, onClick OnSignIn ] [ text "Sign in" ]
                ]
            , p [] [ text model.errorMessage ]
            ]
    }


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { inputUserId : String
    , inputPassword : String
    , errorMessage : String
    }


init : ( Model, Effect Msg )
init =
    ( { inputUserId = ""
      , inputPassword = ""
      , errorMessage = ""
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = ChangeInputUserId String
    | ChangeInputPassword String
    | OnSignIn
    | FailedLogin Json.Value


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ChangeInputUserId v ->
            ( { model | inputUserId = v }, Effect.none )

        ChangeInputPassword v ->
            ( { model | inputPassword = v }, Effect.none )

        OnSignIn ->
            ( model, Effect.fromCmd <| Shared.tryLogin { id = model.inputUserId } )

        FailedLogin _ ->
            ( { model | errorMessage = "ログイン失敗" }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Shared.failedLogin FailedLogin
