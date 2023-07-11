port module Ports.Shared exposing (..)


port signOut : () -> Cmd msg


port succeedSignOut : (() -> msg) -> Sub msg
