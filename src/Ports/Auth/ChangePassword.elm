port module Ports.Auth.ChangePassword exposing (..)

import Domain
import Shared


port changePassword : { newPassword : String } -> Cmd msg


port succeedChangePassword : (Domain.SignInUser -> msg) -> Sub msg


port failChangePassword : (Shared.AuthError -> msg) -> Sub msg
