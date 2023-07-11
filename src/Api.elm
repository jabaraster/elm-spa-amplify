module Api exposing
    ( Kayoinoba
    , KayoinobaId
    , Location
    , Map
    , MapId
    , MapResources
    , PagingList
    , PagingParam
    , Place
    , PlaceId
    , PlaceName
    , ProtectedAccessParam
    , PublicAccessParam
    , RemoteResource
    , continuePagingParam
    , createKayoinoba
    , createMap
    , createPlace
    , defaultKayoinoba
    , defaultLocation
    , defaultMap
    , defaultPlace
    , deleteKayoinoba
    , deleteMap
    , deletePlace
    , doProtectedApi
    , doPublicApi
    , emptyId
    , errorString
    , existsMap
    , fromId
    , fromUrl
    , getMap
    , getMapResources
    , listKayoinobas
    , listMaps
    , listPlaceNames
    , listPlaces
    , mapRemoteResource
    , placeAndKayoinobas
    , placeImageFileName
    , startPagingParam
    , updateKayoinoba
    , updateMap
    , updatePlace
    )

import Api.Enum.PlaceCategory exposing (PlaceCategory(..))
import Api.InputObject exposing (..)
import Api.Mutation
import Api.Object exposing (..)
import Api.Object.Color
import Api.Object.Kayoinoba
import Api.Object.KayoinobaAttributes
import Api.Object.Location
import Api.Object.Map exposing (id)
import Api.Object.ModelKayoinobaConnection
import Api.Object.ModelMapConnection
import Api.Object.ModelPlaceConnection
import Api.Object.Place
import Api.Query
import Api.Scalar exposing (Awsurl(..), Id(..))
import Domain exposing (AppConfig)
import Graphql.Http exposing (..)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe.Extra as Maybe
import RemoteData exposing (RemoteData)


{-| General operation.
-}
type alias PublicAccessParam =
    { graphqlEndpoint : String
    , apiKey : String
    }


type alias ProtectedAccessParam =
    { graphqlEndpoint : String
    , jwtToken : Domain.JwtToken
    }


type alias PagingParam =
    { limit : Int
    , nextToken : Maybe String
    }


type alias PagingList a =
    { nextToken : Maybe String
    , items : List a
    }


emptyId : Api.Scalar.Id
emptyId =
    Api.Scalar.Id ""


startPagingParam : Int -> PagingParam
startPagingParam limit =
    { limit = limit, nextToken = Nothing }


continuePagingParam : PagingParam -> Maybe String -> PagingParam
continuePagingParam param nextToken =
    { param | nextToken = nextToken }


appConfigToPublicAccessParam : AppConfig -> PublicAccessParam
appConfigToPublicAccessParam ac =
    { graphqlEndpoint = ac.graphqlEndpoint
    , apiKey = ac.apiKey
    }


appConfigToProtectedAccessParam : AppConfig -> Maybe ProtectedAccessParam
appConfigToProtectedAccessParam ac =
    Maybe.map
        (\user ->
            { graphqlEndpoint = ac.graphqlEndpoint
            , jwtToken = user.jwtToken
            }
        )
        ac.user


withPublicAccessHeader : PublicAccessParam -> Request decodesTo -> Request decodesTo
withPublicAccessHeader param =
    withHeader "X-API-Key" param.apiKey


withProtectedAccessHeader : ProtectedAccessParam -> Request decodesTo -> Request decodesTo
withProtectedAccessHeader param =
    withHeader "Authorization" param.jwtToken


doPublicApi : AppConfig -> (PublicAccessParam -> Cmd msg) -> Cmd msg
doPublicApi apiConfig operation =
    operation <| appConfigToPublicAccessParam apiConfig


doProtectedApi : AppConfig -> (ProtectedAccessParam -> Cmd msg) -> Cmd msg
doProtectedApi apiConfig operation =
    case appConfigToProtectedAccessParam apiConfig of
        Nothing ->
            Cmd.none

        Just param ->
            operation param


type alias RemoteResource a =
    RemoteData (Graphql.Http.Error a) a


errorString : Graphql.Http.Error a -> String
errorString =
    always "通信に失敗"


type alias Location =
    Api.InputObject.LocationInput


defaultLocation : Location
defaultLocation =
    { latitude = 0, longitude = 0 }


fromId : Id -> String
fromId id =
    case id of
        Id s ->
            s


fromUrl : Awsurl -> String
fromUrl url =
    case url of
        Awsurl s ->
            s


{-| Map operation.
-}
type alias MapId =
    Id


type alias Map =
    { id : MapId
    , name : String
    , title : String
    , topInformation : String
    , zoom : Int
    , centerLocation : Location
    , category1Name : String
    , category2Name : String
    , category3Name : String
    , themeColor : Api.InputObject.ColorInput
    }


defaultMap : Map
defaultMap =
    { id = Id ""
    , name = ""
    , title = ""
    , topInformation = ""
    , centerLocation = defaultLocation
    , zoom = 15
    , category1Name = ""
    , category2Name = ""
    , category3Name = ""
    , themeColor = defaultColor
    }


defaultColor : Api.InputObject.ColorInput
defaultColor =
    { red = 18, green = 17, blue = 65, alpha = 1.0 }


createMap :
    ProtectedAccessParam
    -> Map
    -> (RemoteResource (Maybe Map) -> msg)
    -> Cmd msg
createMap param input handler =
    Api.Mutation.createMap
        identity
        { input =
            Api.InputObject.buildCreateMapInput
                { name = input.name
                , title = input.title
                , topInformation = input.topInformation
                , zoom = input.zoom
                , centerLocation = input.centerLocation
                , category1Name = input.category1Name
                , category2Name = input.category2Name
                , category3Name = input.category3Name
                , themeColor = input.themeColor
                }
                (\src -> { src | id = Present input.id })
        }
        mapSelection
        |> sendMutation param handler


getMap : PublicAccessParam -> MapId -> (RemoteResource (Maybe Map) -> msg) -> Cmd msg
getMap param mapId handler =
    Api.Query.getMap { id = mapId } mapSelection |> sendQuery param handler


existsMap : PublicAccessParam -> MapId -> (RemoteResource (Maybe ()) -> msg) -> Cmd msg
existsMap param mapId handler =
    Api.Query.getMap { id = mapId } (SelectionSet.succeed ()) |> sendQuery param handler


type alias PagingListResponse a =
    { nextToken : Maybe String
    , items : List (Maybe a)
    }


listMaps : PublicAccessParam -> PagingParam -> (RemoteResource (PagingList Map) -> msg) -> Cmd msg
listMaps accessParam pagingParam handler =
    Api.Query.listMaps
        (\optArg ->
            { optArg
                | limit = Present pagingParam.limit
                , nextToken = Maybe.withDefault Null <| Maybe.map Present pagingParam.nextToken
            }
        )
        (SelectionSet.succeed PagingListResponse
            |> with Api.Object.ModelMapConnection.nextToken
            |> with mapsSelection
        )
        |> sendListQuery accessParam handler


mapsSelection : SelectionSet (List (Maybe Map)) Api.Object.ModelMapConnection
mapsSelection =
    Api.Object.ModelMapConnection.items mapSelection


mapSelection : SelectionSet Map Api.Object.Map
mapSelection =
    SelectionSet.succeed Map
        |> with Api.Object.Map.id
        |> with Api.Object.Map.name
        |> with Api.Object.Map.title
        |> with Api.Object.Map.topInformation
        |> with Api.Object.Map.zoom
        |> with (Api.Object.Map.centerLocation locationSelection)
        |> with Api.Object.Map.category1Name
        |> with Api.Object.Map.category2Name
        |> with Api.Object.Map.category3Name
        |> with (Api.Object.Map.themeColor colorSelection)


colorSelection : SelectionSet Api.InputObject.ColorInput Api.Object.Color
colorSelection =
    SelectionSet.succeed Api.InputObject.ColorInput
        |> with Api.Object.Color.red
        |> with Api.Object.Color.green
        |> with Api.Object.Color.blue
        |> with Api.Object.Color.alpha


locationSelection : SelectionSet Api.InputObject.LocationInput Api.Object.Location
locationSelection =
    SelectionSet.succeed Api.InputObject.LocationInput
        |> with Api.Object.Location.latitude
        |> with Api.Object.Location.longitude


updateMap :
    ProtectedAccessParam
    -> Map
    -> (RemoteResource (Maybe Map) -> msg)
    -> Cmd msg
updateMap param input handler =
    Api.Mutation.updateMap
        identity
        { input =
            Api.InputObject.buildUpdateMapInput
                { id = input.id }
                (\src ->
                    { src
                        | name = Present input.name
                        , title = Present input.title
                        , topInformation = Present input.topInformation
                        , zoom = Present input.zoom
                        , centerLocation = Present input.centerLocation
                        , category1Name = Present input.category1Name
                        , category2Name = Present input.category2Name
                        , category3Name = Present input.category3Name
                        , themeColor = Present input.themeColor
                    }
                )
        }
        mapSelection
        |> sendMutation param handler


deleteMap : ProtectedAccessParam -> MapId -> (RemoteResource (Maybe Map) -> msg) -> Cmd msg
deleteMap param mapId handler =
    -- TODO : 下位のデータを同時に削除
    Api.Mutation.deleteMap
        identity
        { input = Api.InputObject.buildDeleteMapInput { id = mapId } }
        mapSelection
        |> sendMutation param handler


{-| Place operation.
-}
type alias PlaceId =
    Id


type alias Place =
    { id : PlaceId
    , name : String
    , address : String
    , category : PlaceCategory
    , contact : String
    , location : Location
    , hasImage : Bool
    }


defaultPlace : Place
defaultPlace =
    { id = Id ""
    , name = ""
    , address = ""
    , category = Category1
    , contact = ""
    , location = defaultLocation
    , hasImage = False
    }


placeImageFileName : Place -> String
placeImageFileName place =
    "placeimage-" ++ fromId place.id


type alias MapResourcesResponse =
    { map : Maybe Map
    , places : Maybe (PagingListResponse Place)
    , kayoinobas : Maybe (PagingListResponse Kayoinoba)
    }


type alias MapResources =
    { map : Map
    , places : PagingList Place
    , kayoinobas : PagingList Kayoinoba
    }


getMapResources :
    PublicAccessParam
    -> MapId
    -> (RemoteResource (Maybe MapResources) -> msg)
    -> Cmd msg
getMapResources param mapId handler =
    (SelectionSet.succeed MapResourcesResponse
        |> with (Api.Query.getMap { id = mapId } mapSelection)
        |> with (listPlacesQuery { limit = 50, nextToken = Nothing } mapId)
        |> with (listKayoinobasQuery { limit = 50, nextToken = Nothing } mapId)
    )
        |> queryRequest param.graphqlEndpoint
        |> withPublicAccessHeader param
        |> send (RemoteData.fromResult >> mapRemoteResource normalizeMapResources >> handler)


normalizeMapResources : MapResourcesResponse -> Maybe MapResources
normalizeMapResources src =
    Maybe.map
        (\map ->
            { map = map
            , places = normalize src.places
            , kayoinobas = normalize src.kayoinobas
            }
        )
        src.map


placeSelection : SelectionSet Place Api.Object.Place
placeSelection =
    SelectionSet.succeed Place
        |> with Api.Object.Place.id
        |> with Api.Object.Place.name
        |> with Api.Object.Place.address
        |> with Api.Object.Place.category
        |> with Api.Object.Place.contact
        |> with (Api.Object.Place.location locationSelection)
        |> with Api.Object.Place.hasImage


createPlace :
    ProtectedAccessParam
    -> MapId
    -> Place
    -> (RemoteResource (Maybe Place) -> msg)
    -> Cmd msg
createPlace param mapId input handler =
    Api.Mutation.createPlace
        identity
        { input =
            Api.InputObject.buildCreatePlaceInput
                { name = input.name
                , address = input.address
                , category = input.category
                , contact = input.contact
                , location = input.location
                , placeMapId = mapId
                , hasImage = input.hasImage
                }
                identity
        }
        placeSelection
        |> sendMutation param handler


updatePlace :
    ProtectedAccessParam
    -> MapId
    -> Place
    -> (RemoteResource (Maybe Place) -> msg)
    -> Cmd msg
updatePlace param mapId input handler =
    Api.Mutation.updatePlace
        identity
        { input =
            Api.InputObject.buildUpdatePlaceInput
                { id = input.id }
                (\src ->
                    { src
                        | name = Present input.name
                        , address = Present input.address
                        , category = Present input.category
                        , contact = Present input.contact
                        , location = Present input.location
                        , placeMapId = Present mapId
                        , hasImage = Present input.hasImage
                    }
                )
        }
        placeSelection
        |> sendMutation param handler


deletePlace : ProtectedAccessParam -> PlaceId -> (RemoteResource (Maybe Place) -> msg) -> Cmd msg
deletePlace param placeId handler =
    -- TODO : 下位のデータを同時に削除
    Api.Mutation.deletePlace
        identity
        { input = Api.InputObject.buildDeletePlaceInput { id = placeId } }
        placeSelection
        |> sendMutation param handler


type alias PlaceName =
    { id : PlaceId
    , name : String
    }


listPlaceNames :
    PublicAccessParam
    -> PagingParam
    -> MapId
    -> (RemoteResource (PagingList PlaceName) -> msg)
    -> Cmd msg
listPlaceNames param pagingParam mapId handler =
    listPlacesQueryCore pagingParam
        mapId
        (SelectionSet.succeed PagingListResponse
            |> with Api.Object.ModelPlaceConnection.nextToken
            |> with
                (Api.Object.ModelPlaceConnection.items
                    (SelectionSet.succeed PlaceName |> with Api.Object.Place.id |> with Api.Object.Place.name)
                )
        )
        |> sendListQuery param handler


listPlaces :
    PublicAccessParam
    -> PagingParam
    -> MapId
    -> (RemoteResource (PagingList Place) -> msg)
    -> Cmd msg
listPlaces param pagingParam mapId handler =
    listPlacesQuery pagingParam mapId |> sendListQuery param handler


listPlacesQuery : PagingParam -> MapId -> SelectionSet (Maybe (PagingListResponse Place)) RootQuery
listPlacesQuery pagingParam mapId =
    listPlacesQueryCore pagingParam
        mapId
        (SelectionSet.succeed PagingListResponse
            |> with Api.Object.ModelPlaceConnection.nextToken
            |> with placesSelection
        )


listPlacesQueryCore :
    PagingParam
    -> MapId
    -> SelectionSet decodesTo ModelPlaceConnection
    -> SelectionSet (Maybe decodesTo) RootQuery
listPlacesQueryCore pagingParam mapId =
    Api.Query.listPlaces
        (\arg ->
            { arg
                | filter =
                    Present <|
                        Api.InputObject.buildModelPlaceFilterInput
                            (\opt ->
                                { opt
                                    | placeMapId =
                                        Present <|
                                            buildModelIDInput
                                                (\id -> { id | eq = Present mapId })
                                }
                            )
                , limit = Present pagingParam.limit
                , nextToken = Maybe.withDefault Null <| Maybe.map Present pagingParam.nextToken
            }
        )


placesSelection : SelectionSet (List (Maybe Place)) Api.Object.ModelPlaceConnection
placesSelection =
    Api.Object.ModelPlaceConnection.items placeSelection


{-| Kayoinoba operation.
|
-}
type alias KayoinobaId =
    Id


type alias KayoinobaAttributes =
    Api.InputObject.KayoinobaAttributesInput


defaultKayoinobaAttributes : KayoinobaAttributes
defaultKayoinobaAttributes =
    { taisou = False
    , noutore = False
    , ongaku = False
    , insyokuari = False
    , undou = False
    , free = False
    }


type alias Kayoinoba =
    { id : KayoinobaId
    , name : String
    , yomigana : String
    , summary : String
    , price : Maybe Int
    , target : String
    , contact : String
    , webSite : Maybe Awsurl
    , kayoinobaPlaceId : PlaceId
    , attributes : KayoinobaAttributes
    , mapIdForSearch : MapId
    }


kayoinobaAttributesSelection : SelectionSet KayoinobaAttributes Api.Object.KayoinobaAttributes
kayoinobaAttributesSelection =
    SelectionSet.succeed Api.InputObject.KayoinobaAttributesInput
        |> with Api.Object.KayoinobaAttributes.taisou
        |> with Api.Object.KayoinobaAttributes.noutore
        |> with Api.Object.KayoinobaAttributes.ongaku
        |> with Api.Object.KayoinobaAttributes.insyokuari
        |> with Api.Object.KayoinobaAttributes.undou
        |> with Api.Object.KayoinobaAttributes.free


defaultKayoinoba : Kayoinoba
defaultKayoinoba =
    { id = Id ""
    , name = ""
    , yomigana = ""
    , summary = ""
    , price = Nothing
    , target = ""
    , contact = ""
    , webSite = Nothing
    , kayoinobaPlaceId = Id ""
    , attributes = defaultKayoinobaAttributes
    , mapIdForSearch = Id ""
    }


kayoinobaSelection : SelectionSet Kayoinoba Api.Object.Kayoinoba
kayoinobaSelection =
    SelectionSet.succeed Kayoinoba
        |> with Api.Object.Kayoinoba.id
        |> with Api.Object.Kayoinoba.name
        |> with Api.Object.Kayoinoba.yomigana
        |> with Api.Object.Kayoinoba.summary
        |> with Api.Object.Kayoinoba.price
        |> with Api.Object.Kayoinoba.target
        |> with Api.Object.Kayoinoba.contact
        |> with Api.Object.Kayoinoba.webSite
        |> with Api.Object.Kayoinoba.kayoinobaPlaceId
        |> with (Api.Object.Kayoinoba.attributes kayoinobaAttributesSelection)
        |> with Api.Object.Kayoinoba.mapIdForSearch


type alias PlaceAndKayoinobas =
    { mapId : MapId
    , place : Place
    , kayoinobas : PagingList Kayoinoba
    }


createKayoinoba :
    ProtectedAccessParam
    -> Kayoinoba
    -> (RemoteResource (Maybe Kayoinoba) -> msg)
    -> Cmd msg
createKayoinoba param input handler =
    Api.Mutation.createKayoinoba
        identity
        { input =
            Api.InputObject.buildCreateKayoinobaInput
                { name = input.name
                , yomigana = input.yomigana
                , summary = input.summary
                , target = input.target
                , contact = input.contact
                , attributes = input.attributes
                , kayoinobaPlaceId = input.kayoinobaPlaceId
                , mapIdForSearch = input.mapIdForSearch
                }
                (\src ->
                    { src
                        | price = OptionalArgument.fromMaybe input.price
                        , webSite = OptionalArgument.fromMaybe input.webSite
                    }
                )
        }
        kayoinobaSelection
        |> sendMutation param handler


updateKayoinoba :
    ProtectedAccessParam
    -> Kayoinoba
    -> (RemoteResource (Maybe Kayoinoba) -> msg)
    -> Cmd msg
updateKayoinoba param input handler =
    Api.Mutation.updateKayoinoba
        identity
        { input =
            Api.InputObject.buildUpdateKayoinobaInput
                { id = input.id }
                (\src ->
                    { src
                        | name = Present input.name
                        , yomigana = Present input.yomigana
                        , summary = Present input.summary
                        , price = OptionalArgument.fromMaybe input.price
                        , target = Present input.target
                        , webSite = OptionalArgument.fromMaybe input.webSite
                        , attributes = Present input.attributes
                        , kayoinobaPlaceId = Present input.kayoinobaPlaceId
                    }
                )
        }
        kayoinobaSelection
        |> sendMutation param handler


deleteKayoinoba : ProtectedAccessParam -> KayoinobaId -> (RemoteResource (Maybe Kayoinoba) -> msg) -> Cmd msg
deleteKayoinoba param kayoinobaId handler =
    Api.Mutation.deleteKayoinoba
        identity
        { input = Api.InputObject.buildDeleteKayoinobaInput { id = kayoinobaId } }
        kayoinobaSelection
        |> sendMutation param handler


type alias PlaceResponse =
    { place : Place
    , map : MapIdOnly
    }


type alias MapIdOnly =
    { id : MapId
    }


placeResponseSelection : SelectionSet PlaceResponse Api.Object.Place
placeResponseSelection =
    SelectionSet.succeed PlaceResponse
        |> with placeSelection
        |> with (Api.Object.Place.map (SelectionSet.succeed MapIdOnly |> with Api.Object.Map.id))


type alias PlaceAndKayoinobasResponse =
    { place : Maybe PlaceResponse
    , kayoinobas : Maybe (PagingListResponse Kayoinoba)
    }


placeAndKayoinobas :
    PublicAccessParam
    -> PlaceId
    -> PagingParam
    -> MapId
    -> (RemoteResource (Maybe PlaceAndKayoinobas) -> msg)
    -> Cmd msg
placeAndKayoinobas param placeId pagingParam mapId handler =
    (SelectionSet.succeed PlaceAndKayoinobasResponse
        |> with (Api.Query.getPlace { id = placeId } placeResponseSelection)
        |> with (listKayoinobasQuery pagingParam mapId)
    )
        |> queryRequest param.graphqlEndpoint
        |> withPublicAccessHeader param
        |> send (RemoteData.fromResult >> mapRemoteResource normalizePlaceAndKayoinobas >> handler)


normalizePlaceAndKayoinobas : PlaceAndKayoinobasResponse -> Maybe PlaceAndKayoinobas
normalizePlaceAndKayoinobas src =
    Maybe.map
        (\placeRes ->
            { mapId = placeRes.map.id, place = placeRes.place, kayoinobas = normalize src.kayoinobas }
        )
        src.place


listKayoinobas :
    PublicAccessParam
    -> PagingParam
    -> MapId
    -> (RemoteResource (PagingList Kayoinoba) -> msg)
    -> Cmd msg
listKayoinobas param pagingParam mapId handler =
    listKayoinobasQuery pagingParam mapId |> sendListQuery param handler


listKayoinobasQuery :
    PagingParam
    -> MapId
    -> SelectionSet (Maybe (PagingListResponse Kayoinoba)) RootQuery
listKayoinobasQuery pagingParam mapId =
    Api.Query.listKayoinobas
        (\arg ->
            { arg
                | filter =
                    Present <|
                        buildModelKayoinobaFilterInput
                            (\opt -> { opt | mapIdForSearch = Present <| buildModelIDInput (\src -> { src | eq = Present mapId }) })
                , limit = Present pagingParam.limit
                , nextToken = Maybe.withDefault Null <| Maybe.map Present pagingParam.nextToken
            }
        )
        (SelectionSet.succeed PagingListResponse
            |> with Api.Object.ModelKayoinobaConnection.nextToken
            |> with kayoinobasSelection
        )


kayoinobasSelection : SelectionSet (List (Maybe Kayoinoba)) Api.Object.ModelKayoinobaConnection
kayoinobasSelection =
    Api.Object.ModelKayoinobaConnection.items kayoinobaSelection


{-| Utility.
|
-}
sendListQuery :
    PublicAccessParam
    -> (RemoteResource (PagingList a) -> msg)
    -> SelectionSet (Maybe (PagingListResponse a)) RootQuery
    -> Cmd msg
sendListQuery param handler =
    queryRequest param.graphqlEndpoint
        >> withPublicAccessHeader param
        >> send
            (RemoteData.fromResult
                >> mapRemoteResource normalize
                >> handler
            )


normalize : Maybe (PagingListResponse a) -> PagingList a
normalize =
    h >> j


h : Maybe (PagingListResponse a) -> Maybe (PagingList a)
h =
    Maybe.map (\src -> { nextToken = src.nextToken, items = Maybe.values src.items })


j : Maybe (PagingList a) -> PagingList a
j =
    Maybe.withDefault
        { nextToken = Nothing, items = [] }


sendQuery :
    PublicAccessParam
    -> (RemoteResource a -> msg)
    -> SelectionSet a RootQuery
    -> Cmd msg
sendQuery param handler =
    queryRequest param.graphqlEndpoint
        >> withPublicAccessHeader param
        >> send (RemoteData.fromResult >> handler)


sendMutation :
    ProtectedAccessParam
    -> (RemoteResource a -> msg)
    -> SelectionSet a RootMutation
    -> Cmd msg
sendMutation param handler =
    mutationRequest param.graphqlEndpoint
        >> withProtectedAccessHeader param
        >> send (RemoteData.fromResult >> handler)


mapRemoteResource : (a -> b) -> RemoteData (Graphql.Http.Error a) a -> RemoteData (Graphql.Http.Error b) b
mapRemoteResource f =
    RemoteData.mapBoth f (Graphql.Http.mapError f)
