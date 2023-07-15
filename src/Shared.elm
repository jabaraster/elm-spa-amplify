port module Shared exposing
    ( AuthError
    , Flags
    , Model
    , Msg(..)
    , init
    , signOut
    , subscriptions
    , update
    )

import Domain
import Gen.Route
import Json.Encode as Json
import Platform.Cmd as Cmd
import Request exposing (Request)
import Url exposing (Protocol(..), Url)


type alias AuthError =
    { code : String
    , message : String
    }


type alias Flags =
    Domain.AppConfig


type alias Model =
    Domain.AppConfig


type Msg
    = SucceedSignIn Domain.SignInUser
    | RedirectToSignIn Json.Value
    | SignedOut Json.Value


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( flags
    , Cmd.none
    )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        SucceedSignIn user ->
            ( { model | user = Just user }
            , pushRoute Gen.Route.Admin__Map req
            )

        RedirectToSignIn _ ->
            ( model
            , pushRoute Gen.Route.Auth__SignIn req
            )

        SignedOut _ ->
            ( model
            , pushRoute Gen.Route.Auth__SignIn req
            )


pushRoute : Gen.Route.Route -> Request -> Cmd msg
pushRoute route req =
    if req.route /= route then
        Request.pushRoute route req

    else
        Cmd.none


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ redirectToSignIn RedirectToSignIn
        , signedOut SignedOut
        ]


port signOut : () -> Cmd msg


port succeedSignOut : (() -> msg) -> Sub msg


port redirectToSignIn : (Json.Value -> msg) -> Sub msg


port signedOut : (Json.Value -> msg) -> Sub msg
