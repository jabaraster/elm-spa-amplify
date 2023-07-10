port module Shared exposing
    ( Flags
    , Model
    , Msg
    , failedLogin
    , init
    , subscriptions
    , succeededLogin
    , tryLogin
    , update
    )

import Domain
import Gen.Route
import Json.Encode as Json
import Request exposing (Request)


type alias PlaceIconUrls =
    { category1 : String
    , category2 : String
    , category3 : String
    }


type alias KayoinobaAttributeIconUrls =
    { taisou : String
    , noutore : String
    , ongaku : String
    , insyokuari : String
    , undou : String
    , free : String
    }


type alias Flags =
    { placeIconUrls : PlaceIconUrls
    , kayoinobaAttributeIconUrls : KayoinobaAttributeIconUrls
    , user : Maybe Domain.User
    }


type alias Model =
    Flags


type Msg
    = SucceededLogin Domain.User


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( flags
    , Cmd.none
    )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        SucceededLogin user ->
            ( { model | user = Just user }
            , if Gen.Route.Auth__SignIn == req.route then
                Request.pushRoute Gen.Route.Admin__Map req

              else
                Cmd.none
            )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    succeededLogin SucceededLogin


port tryLogin : Domain.User -> Cmd msg


port succeededLogin : (Domain.User -> msg) -> Sub msg


port failedLogin : (Json.Value -> msg) -> Sub msg
