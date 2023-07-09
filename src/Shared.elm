module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    )

import Json.Decode as Json
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
    }


type alias Model =
    { placeIconUrls : PlaceIconUrls
    , kayoinobaAttributeIconUrls : KayoinobaAttributeIconUrls
    }


type Msg
    = NoOp


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( { placeIconUrls = flags.placeIconUrls
      , kayoinobaAttributeIconUrls = flags.kayoinobaAttributeIconUrls
      }
    , Cmd.none
    )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
