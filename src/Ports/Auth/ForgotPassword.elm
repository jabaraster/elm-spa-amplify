port module Ports.Auth.ForgotPassword exposing (..)

import Json.Encode as Json
import Shared


port forgotPassword : { userId : String } -> Cmd msg


port succeedForgotPassword : (Json.Value -> msg) -> Sub msg


port failForgotPassword : (Shared.AuthError -> msg) -> Sub msg
