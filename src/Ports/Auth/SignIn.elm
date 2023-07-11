port module Ports.Auth.SignIn exposing (..)

import Domain
import Json.Encode as Json
import Shared


port signIn :
    { userId : String
    , password : String
    }
    -> Cmd msg


port succeedSignIn : (Domain.SignInUser -> msg) -> Sub msg


port failSignIn : (Shared.AuthError -> msg) -> Sub msg


port requireChangePassword : (Json.Value -> msg) -> Sub msg
