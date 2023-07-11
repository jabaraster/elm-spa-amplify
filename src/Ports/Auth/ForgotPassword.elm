port module Ports.Auth.ForgotPassword exposing (..)

import Json.Encode as Json


port forgotPassword : { userId : String } -> Cmd msg


port succeedForgotPassword : (Json.Value -> msg) -> Sub msg


port failForgotPassword : (Json.Value -> msg) -> Sub msg
