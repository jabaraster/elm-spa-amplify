port module Ports exposing
    ( FileNameToUrl
    , receiveFileUrl
    , receiveFileUrls
    , requestFileUrl
    , requestFileUrls
    , startGoogleMap
    , uploadToStorage
    )

import Json.Encode exposing (Value)


port startGoogleMap : Value -> Cmd msg


port uploadToStorage :
    { fileName : String
    , dataUrl : String
    , contentType : String
    }
    -> Cmd msg


port requestFileUrl : String -> Cmd msg


port receiveFileUrl : (String -> msg) -> Sub msg


port requestFileUrls : List String -> Cmd msg


type alias FileNameToUrl =
    { fileName : String
    , url : String
    }


port receiveFileUrls : (List FileNameToUrl -> msg) -> Sub msg
