module RemoteResourceLoader exposing
    ( RemoteResourceLoader
    , empty
    , isAllLoaded
    , list
    , load
    , new
    )

import Api exposing (PagingList, PagingParam, RemoteResource)
import RemoteData exposing (RemoteData(..))


type RemoteResourceLoader a msg
    = RemoteResourceLoader
        { loadOperation : PagingParam -> (RemoteResource (PagingList a) -> msg) -> Cmd msg
        , failHandler : RemoteResource (PagingList a) -> msg
        , nextToken : Maybe String
        , loadMore : RemoteResourceLoader a msg -> msg
        , completion : RemoteResourceLoader a msg -> msg
        , limit : Int
        , pool : List a
        }
    | Empty msg


empty : msg -> RemoteResourceLoader a msg
empty nop =
    Empty nop


new :
    (PagingParam -> (RemoteResource (PagingList a) -> msg) -> Cmd msg)
    -> (RemoteResourceLoader a msg -> msg)
    -> (RemoteResource (PagingList a) -> msg)
    -> (RemoteResourceLoader a msg -> msg)
    -> RemoteResourceLoader a msg
new loadOperation loadMore failHandler completion =
    RemoteResourceLoader
        { loadOperation = loadOperation
        , failHandler = failHandler
        , nextToken = Nothing
        , loadMore = loadMore
        , completion = completion
        , limit = 50
        , pool = []
        }


load : RemoteResourceLoader a msg -> Cmd msg
load loader =
    case loader of
        RemoteResourceLoader { loadOperation, limit, nextToken } ->
            loadOperation { nextToken = nextToken, limit = limit } (handleLoaded loader)

        Empty _ ->
            Cmd.none


handleLoaded : RemoteResourceLoader a msg -> RemoteResource (PagingList a) -> msg
handleLoaded loader res =
    case res of
        Success { nextToken, items } ->
            let
                ls =
                    pool_ loader ++ items
            in
            case ( nextToken, loader ) of
                ( _, Empty nop ) ->
                    nop

                ( Nothing, RemoteResourceLoader p ) ->
                    completion_ loader <| RemoteResourceLoader { p | pool = ls, nextToken = Nothing }

                ( Just token, RemoteResourceLoader p ) ->
                    loadMore_ loader <| RemoteResourceLoader { p | pool = ls, nextToken = Just token }

        p ->
            failHandler_ loader p


failHandler_ : RemoteResourceLoader a msg -> (RemoteResource (PagingList a) -> msg)
failHandler_ loader =
    case loader of
        RemoteResourceLoader { failHandler } ->
            failHandler

        Empty nop ->
            always nop


loadMore_ : RemoteResourceLoader a msg -> RemoteResourceLoader a msg -> msg
loadMore_ loader =
    case loader of
        RemoteResourceLoader { loadMore } ->
            loadMore

        Empty nop ->
            always nop


limit_ : RemoteResourceLoader a msg -> Int
limit_ loader =
    case loader of
        RemoteResourceLoader { limit } ->
            limit

        Empty _ ->
            0


pool_ : RemoteResourceLoader a msg -> List a
pool_ loader =
    case loader of
        RemoteResourceLoader { pool } ->
            pool

        Empty _ ->
            []


completion_ : RemoteResourceLoader a msg -> RemoteResourceLoader a msg -> msg
completion_ loader =
    case loader of
        RemoteResourceLoader { completion } ->
            completion

        Empty nop ->
            always nop


isAllLoaded : RemoteResourceLoader a msg -> Bool
isAllLoaded loader =
    case loader of
        RemoteResourceLoader { nextToken } ->
            case nextToken of
                Nothing ->
                    True

                Just _ ->
                    False

        Empty _ ->
            False


list : RemoteResourceLoader a msg -> List a
list =
    pool_
