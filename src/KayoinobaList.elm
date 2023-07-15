module KayoinobaList exposing
    ( Filter
    , KayoinobaList
    , defaultFilter
    , empty
    , filtered
    , kayoinobaFromId
    , kayoinobas
    , kayoinobasForPlace
    , new
    , placeForKayoinoba
    , places
    )

import Api exposing (Kayoinoba, KayoinobaId, Place)
import Api.Enum.PlaceCategory exposing (PlaceCategory(..))
import Api.InputObject exposing (KayoinobaAttributesInput)
import Dict exposing (Dict)
import List.Extra as List
import Set


type alias Filter =
    { text : String
    , placeCategory :
        { category1 : Bool
        , category2 : Bool
        , category3 : Bool
        }
    , kayoinobaAttributes : KayoinobaAttributesInput
    }


allFilter : Filter
allFilter =
    let
        pf =
            defaultFilter.placeCategory
    in
    { defaultFilter | placeCategory = { pf | category2 = True, category3 = True } }


defaultFilter : Filter
defaultFilter =
    { text = ""
    , placeCategory =
        { category1 = True
        , category2 = False
        , category3 = False
        }
    , kayoinobaAttributes =
        { taisou = True
        , noutore = True
        , ongaku = True
        , insyokuari = True
        , undou = True
        , free = True
        }
    }


type KayoinobaList
    = KayoinobaList
        { placeIdToPlace_ : Dict String Place -- Kayoinoba構造体の親Placeにすぐアクセスするためのインデックス
        , placeIdToKayoinobas_ : Dict String (List Kayoinoba)
        , places_ : List Place -- Kayoinobaから参照されているPlaceのリスト. 不要なPlaceが掃除されている.
        , kayoinobas_ : List Kayoinoba
        }


empty : KayoinobaList
empty =
    KayoinobaList
        { placeIdToPlace_ = Dict.empty
        , placeIdToKayoinobas_ = Dict.empty
        , places_ = []
        , kayoinobas_ = []
        }


new : List Place -> List Kayoinoba -> KayoinobaList
new ps ks =
    new_ ps ks allFilter


filtered : Filter -> KayoinobaList -> KayoinobaList
filtered filter (KayoinobaList { places_, kayoinobas_ }) =
    new_ places_ kayoinobas_ filter


places : KayoinobaList -> List Place
places (KayoinobaList { places_ }) =
    places_


placeForKayoinoba : Kayoinoba -> KayoinobaList -> Maybe Place
placeForKayoinoba kayoinoba (KayoinobaList { placeIdToPlace_ }) =
    Dict.get (Api.fromId kayoinoba.kayoinobaPlaceId) placeIdToPlace_


kayoinobas : KayoinobaList -> List Kayoinoba
kayoinobas (KayoinobaList { kayoinobas_ }) =
    kayoinobas_


kayoinobaFromId : KayoinobaId -> KayoinobaList -> Maybe Kayoinoba
kayoinobaFromId kayoinobaId (KayoinobaList { kayoinobas_ }) =
    List.find (\k -> k.id == kayoinobaId) kayoinobas_


kayoinobasForPlace : Place -> KayoinobaList -> List Kayoinoba
kayoinobasForPlace place (KayoinobaList { placeIdToKayoinobas_ }) =
    Maybe.withDefault [] <| Dict.get (Api.fromId place.id) placeIdToKayoinobas_


new_ : List Place -> List Kayoinoba -> Filter -> KayoinobaList
new_ ps ks filter =
    let
        ps_ =
            List.filter (filterPlaceText filter) <| List.filter (filterPlaceCategory filter) ps

        ks_ =
            List.filter (filterKayoinobaAttributes filter) ks
    in
    newCore ps_ ks_


newCore : List Place -> List Kayoinoba -> KayoinobaList
newCore ps ks =
    let
        -- 親であるPlaceがpsにないKayoinobaを除く
        psIds =
            Set.fromList <| List.map (Api.fromId << .id) ps

        cleanKayoinobas =
            List.filter (\k -> Set.member (Api.fromId k.kayoinobaPlaceId) psIds) ks

        -- 子であるKayoinobaが１つもないPlaceを除く
        kayoinobaPlaceIds =
            Set.fromList <| List.map (Api.fromId << .kayoinobaPlaceId) cleanKayoinobas

        cleanPlaces =
            List.filter (\place -> Set.member (Api.fromId place.id) kayoinobaPlaceIds) ps
    in
    KayoinobaList
        { placeIdToPlace_ = Dict.fromList <| List.map (\place -> ( Api.fromId place.id, place )) cleanPlaces
        , placeIdToKayoinobas_ =
            List.foldl
                insertOrConcat
                Dict.empty
                cleanKayoinobas
        , places_ = cleanPlaces
        , kayoinobas_ = cleanKayoinobas
        }


filterPlaceCategory : Filter -> Place -> Bool
filterPlaceCategory filter place =
    if filter.placeCategory.category1 && place.category == Category1 then
        True

    else if filter.placeCategory.category2 && place.category == Category2 then
        True

    else if filter.placeCategory.category3 && place.category == Category3 then
        True

    else
        False


filterPlaceText : Filter -> Place -> Bool
filterPlaceText { text } place =
    case text of
        "" ->
            True

        _ ->
            String.contains text place.name
                || String.contains text place.address


filterKayoinobaAttributes : Filter -> Kayoinoba -> Bool
filterKayoinobaAttributes filter kayoinoba =
    if filter.kayoinobaAttributes.taisou && kayoinoba.attributes.taisou then
        True

    else if filter.kayoinobaAttributes.noutore && kayoinoba.attributes.noutore then
        True

    else if filter.kayoinobaAttributes.ongaku && kayoinoba.attributes.ongaku then
        True

    else if filter.kayoinobaAttributes.insyokuari && kayoinoba.attributes.insyokuari then
        True

    else if filter.kayoinobaAttributes.undou && kayoinoba.attributes.undou then
        True

    else if filter.kayoinobaAttributes.free && kayoinoba.attributes.free then
        True

    else
        False


kayoinobasInPlace : Place -> KayoinobaList -> List Kayoinoba
kayoinobasInPlace place list =
    case list of
        KayoinobaList { placeIdToKayoinobas_ } ->
            case Dict.get (Api.fromId place.id) placeIdToKayoinobas_ of
                Nothing ->
                    []

                Just ks ->
                    ks


insertOrConcat : Kayoinoba -> Dict String (List Kayoinoba) -> Dict String (List Kayoinoba)
insertOrConcat kayoinoba acc =
    let
        kayoinobaPlaceId =
            Api.fromId kayoinoba.kayoinobaPlaceId
    in
    case Dict.get (Api.fromId kayoinoba.kayoinobaPlaceId) acc of
        Nothing ->
            Dict.insert kayoinobaPlaceId [ kayoinoba ] acc

        Just _ ->
            Dict.update kayoinobaPlaceId (Maybe.map (\now -> kayoinoba :: now)) acc
