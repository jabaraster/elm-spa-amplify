module Route exposing (adminKayoinobaHref, adminMapHref, adminPlaceHref)

import Api exposing (MapId)
import Gen.Route


adminKayoinobaHref : Maybe MapId -> String
adminKayoinobaHref mMapId =
    Gen.Route.toHref Gen.Route.Admin__Kayoinoba
        ++ (Maybe.withDefault "" <|
                Maybe.map (\mapId -> "?map-id=" ++ Api.fromId mapId) mMapId
           )


adminMapHref : String
adminMapHref =
    Gen.Route.toHref Gen.Route.Admin__Map


adminPlaceHref : Maybe MapId -> String
adminPlaceHref mMapId =
    Gen.Route.toHref Gen.Route.Admin__Place
        ++ (Maybe.withDefault "" <|
                Maybe.map (\mapId -> "?map-id=" ++ Api.fromId mapId) mMapId
           )
