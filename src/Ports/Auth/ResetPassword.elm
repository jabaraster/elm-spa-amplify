port module Ports.Auth.ResetPassword exposing (..)

import Json.Encode as Json
import Shared


port resetPassword :
    { userId : String
    , code : String
    , password : String
    }
    -> Cmd msg


port succeedResetPassword : (Json.Value -> msg) -> Sub msg


port failResetPassword : (Shared.AuthError -> msg) -> Sub msg
