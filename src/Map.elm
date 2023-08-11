module Map exposing (..)

import Api exposing (Kayoinoba, KayoinobaId, Map, MapId, PagingList, Place, PublicAccessParam, RemoteResource)
import Api.Enum.PlaceCategory exposing (PlaceCategory(..))
import Api.Scalar
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Navigation
import Bulma.Classes as B
import Css exposing (..)
import Css.Media as Media exposing (MediaQuery)
import Dict exposing (Dict)
import Domain exposing (AppConfig, KayoinobaAttributeIconUrls, PlaceIconUrls)
import GoogleMaps.Map as GoogleMaps
import GoogleMaps.Marker as Marker exposing (Marker)
import Html.Styled as H exposing (Html, a, div, h2, img, li, ol, span, text, ul)
import Html.Styled.Attributes as A exposing (..)
import Html.Styled.Events exposing (..)
import KayoinobaList exposing (Filter, KayoinobaList)
import Ports exposing (FileNameToUrl)
import RemoteData exposing (RemoteData(..))
import RemoteResourceLoader exposing (RemoteResourceLoader)
import Styles
import Tags exposing (..)
import Url exposing (Protocol(..), Url)
import Url.Parser as UP exposing ((</>))
import Views exposing (IconKind(..))


type alias Model =
    { apiParam : PublicAccessParam
    , route : Route
    , key : Navigation.Key
    , map : Maybe Map
    , kayoinobaList : KayoinobaList
    , filter : Filter
    , filtered : KayoinobaList
    , filterControlVisible : Bool
    , googleMapApiKey : String
    , placeIconUrls : PlaceIconUrls
    , kayoinobaAttributeIconUrls : KayoinobaAttributeIconUrls
    , placeLoader : RemoteResourceLoader Place Msg
    , kayoinobaLoader : RemoteResourceLoader Kayoinoba Msg
    , kayoinobaListVisible : Bool
    , fileNameToUrl : Dict String String
    , showingKayoinobaDetail : Maybe Kayoinoba
    }


type Route
    = MapPage MapId
    | KayoinobaPage MapId KayoinobaId
    | NotFoundPage


parseUrl : Url -> Route
parseUrl url =
    let
        mapParser =
            UP.s "map" </> UP.map Api.Scalar.Id UP.string

        parser =
            UP.oneOf
                [ UP.map MapPage mapParser
                , UP.map KayoinobaPage <| mapParser </> UP.s "kayoinoba" </> UP.map Api.Scalar.Id UP.string
                ]
    in
    Maybe.withDefault NotFoundPage <| UP.parse parser url


buildKayoinobaUrl : MapId -> KayoinobaId -> String
buildKayoinobaUrl mapId kayoinobaId =
    buildMapUrl mapId ++ "/kayoinoba/" ++ Api.fromId kayoinobaId


buildMapUrl : MapId -> String
buildMapUrl mapId =
    "/map/" ++ Api.fromId mapId


routeToMapId : Route -> Maybe MapId
routeToMapId r =
    case r of
        MapPage mapId ->
            Just mapId

        KayoinobaPage mapId _ ->
            Just mapId

        NotFoundPage ->
            Nothing


init : AppConfig -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init appConfig url key =
    let
        route =
            parseUrl url

        mMapId =
            routeToMapId route

        apiParam =
            { graphqlEndpoint = appConfig.graphqlEndpoint, apiKey = appConfig.apiKey }

        ( placeLoader, kayoinobaLoader ) =
            case mMapId of
                Nothing ->
                    ( RemoteResourceLoader.empty Nop, RemoteResourceLoader.empty Nop )

                Just mapId ->
                    ( RemoteResourceLoader.new
                        (\pagingParam -> Api.listPlaces apiParam pagingParam mapId)
                        LoadMorePlaces
                        FailLoadPlaces
                        LoadedAllPlaces
                    , RemoteResourceLoader.new
                        (\pagingParam -> Api.listKayoinobas apiParam pagingParam mapId)
                        LoadMoreKayoinobas
                        FailLoadKayoinobas
                        LoadedAllKayoinobas
                    )
    in
    ( { apiParam = apiParam
      , route = route
      , key = key
      , map = Nothing
      , kayoinobaList = KayoinobaList.empty
      , filter = KayoinobaList.defaultFilter
      , filtered = KayoinobaList.empty
      , googleMapApiKey = appConfig.googleMapApiKey
      , placeLoader = placeLoader
      , kayoinobaLoader = kayoinobaLoader
      , kayoinobaListVisible = False
      , placeIconUrls = appConfig.placeIconUrls
      , kayoinobaAttributeIconUrls = appConfig.kayoinobaAttributeIconUrls
      , filterControlVisible = False
      , fileNameToUrl = Dict.empty
      , showingKayoinobaDetail = Nothing
      }
    , case mMapId of
        Nothing ->
            Cmd.none

        Just mapId ->
            Api.getMap apiParam mapId GotMap
    )


main : Program AppConfig Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveFileUrls ReceiveFileUrls


filtered : Filter -> Model -> Model
filtered newFilter model =
    { model | filter = newFilter, filtered = KayoinobaList.filtered newFilter model.kayoinobaList }


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | GotMap (RemoteResource (Maybe Map))
    | ToggleFilterControl
    | ShowKayoinobaList
    | ShowKayoinobaDetail Kayoinoba
    | TogglePlaceCategoryFilter PlaceCategory
    | ToggleKATaisou
    | ToggleKANoutore
    | ToggleKAOngaku
    | ToggleKAInsyokuari
    | ToggleKAUndou
    | ToggleKAFree
    | ToggleAllPlaceCategories
    | ToggleAllKayoinobaAttributes
    | HideFilterControl
    | HideKayoinobaList
    | ChangeInput (Model -> String -> Model) String
    | LoadedAllPlaces (RemoteResourceLoader Place Msg)
    | LoadMorePlaces (RemoteResourceLoader Place Msg)
    | FailLoadPlaces (RemoteResource (PagingList Place))
    | LoadedAllKayoinobas (RemoteResourceLoader Kayoinoba Msg)
    | LoadMoreKayoinobas (RemoteResourceLoader Kayoinoba Msg)
    | FailLoadKayoinobas (RemoteResource (PagingList Kayoinoba))
    | ReceiveFileUrls (List FileNameToUrl)
    | HideKayoinobaDetail
    | Nop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadMorePlaces loader ->
            ( { model | placeLoader = loader }, RemoteResourceLoader.load loader )

        LoadMoreKayoinobas loader ->
            ( { model | kayoinobaLoader = loader }, RemoteResourceLoader.load loader )

        LoadedAllPlaces loader ->
            case ( RemoteResourceLoader.isAllLoaded loader, RemoteResourceLoader.isAllLoaded model.kayoinobaLoader ) of
                ( True, True ) ->
                    let
                        ( newModel, cmd ) =
                            allLoaded model (RemoteResourceLoader.list loader) (RemoteResourceLoader.list model.kayoinobaLoader)
                    in
                    ( { newModel | placeLoader = loader }, cmd )

                _ ->
                    ( { model | placeLoader = loader }, Cmd.none )

        LoadedAllKayoinobas loader ->
            case ( RemoteResourceLoader.isAllLoaded loader, RemoteResourceLoader.isAllLoaded model.placeLoader ) of
                ( True, True ) ->
                    let
                        ( newModel, cmd ) =
                            allLoaded model (RemoteResourceLoader.list model.placeLoader) (RemoteResourceLoader.list loader)
                    in
                    ( { newModel | kayoinobaLoader = loader }, cmd )

                _ ->
                    ( { model | kayoinobaLoader = loader }, Cmd.none )

        FailLoadPlaces _ ->
            fail model

        FailLoadKayoinobas _ ->
            fail model

        GotMap (Success (Just map)) ->
            ( { model | map = Just map }
            , Cmd.batch
                [ RemoteResourceLoader.load model.placeLoader
                , RemoteResourceLoader.load model.kayoinobaLoader
                ]
            )

        GotMap _ ->
            fail model

        ToggleFilterControl ->
            ( { model | filterControlVisible = not model.filterControlVisible }, Cmd.none )

        ShowKayoinobaList ->
            ( { model | kayoinobaListVisible = True }, Cmd.none )

        ShowKayoinobaDetail kayoinoba ->
            case routeToMapId model.route of
                Nothing ->
                    fail model

                Just mapId ->
                    ( model, Navigation.pushUrl model.key <| buildKayoinobaUrl mapId kayoinoba.id )

        HideKayoinobaDetail ->
            case routeToMapId model.route of
                Nothing ->
                    fail model

                Just mapId ->
                    ( model, Navigation.pushUrl model.key <| buildMapUrl mapId )

        HideKayoinobaList ->
            ( { model | kayoinobaListVisible = False }, Cmd.none )

        TogglePlaceCategoryFilter category ->
            let
                filter =
                    model.filter

                cat =
                    filter.placeCategory

                newFilter =
                    case category of
                        Category1 ->
                            { filter | placeCategory = { cat | category1 = not cat.category1 } }

                        Category2 ->
                            { filter | placeCategory = { cat | category2 = not cat.category2 } }

                        Category3 ->
                            { filter | placeCategory = { cat | category3 = not cat.category3 } }
            in
            ( filtered newFilter model, Cmd.none )

        ToggleKATaisou ->
            let
                filter =
                    model.filter

                ka =
                    filter.kayoinobaAttributes

                newFilter =
                    { filter | kayoinobaAttributes = { ka | taisou = not ka.taisou } }
            in
            ( filtered newFilter model, Cmd.none )

        ToggleKANoutore ->
            let
                filter =
                    model.filter

                ka =
                    filter.kayoinobaAttributes

                newFilter =
                    { filter | kayoinobaAttributes = { ka | noutore = not ka.noutore } }
            in
            ( filtered newFilter model, Cmd.none )

        ToggleKAOngaku ->
            let
                filter =
                    model.filter

                ka =
                    filter.kayoinobaAttributes

                newFilter =
                    { filter | kayoinobaAttributes = { ka | ongaku = not ka.ongaku } }
            in
            ( filtered newFilter model, Cmd.none )

        ToggleKAInsyokuari ->
            let
                filter =
                    model.filter

                ka =
                    filter.kayoinobaAttributes

                newFilter =
                    { filter | kayoinobaAttributes = { ka | insyokuari = not ka.insyokuari } }
            in
            ( filtered newFilter model, Cmd.none )

        ToggleKAUndou ->
            let
                filter =
                    model.filter

                ka =
                    filter.kayoinobaAttributes

                newFilter =
                    { filter | kayoinobaAttributes = { ka | undou = not ka.undou } }
            in
            ( filtered newFilter model, Cmd.none )

        ToggleKAFree ->
            let
                filter =
                    model.filter

                ka =
                    filter.kayoinobaAttributes

                newFilter =
                    { filter | kayoinobaAttributes = { ka | free = not ka.free } }
            in
            ( filtered newFilter model, Cmd.none )

        ToggleAllPlaceCategories ->
            let
                filter =
                    model.filter

                newFilter =
                    if
                        filter.placeCategory.category1
                            && filter.placeCategory.category2
                            && filter.placeCategory.category3
                    then
                        { filter | placeCategory = { category1 = False, category2 = False, category3 = False } }

                    else
                        { filter | placeCategory = { category1 = True, category2 = True, category3 = True } }
            in
            ( filtered newFilter model, Cmd.none )

        ToggleAllKayoinobaAttributes ->
            let
                filter =
                    model.filter

                newFilter =
                    if
                        filter.kayoinobaAttributes.taisou
                            && filter.kayoinobaAttributes.noutore
                            && filter.kayoinobaAttributes.ongaku
                            && filter.kayoinobaAttributes.insyokuari
                            && filter.kayoinobaAttributes.undou
                            && filter.kayoinobaAttributes.free
                    then
                        { filter
                            | kayoinobaAttributes =
                                { taisou = False
                                , noutore = False
                                , ongaku = False
                                , insyokuari = False
                                , undou = False
                                , free = False
                                }
                        }

                    else
                        { filter
                            | kayoinobaAttributes =
                                { taisou = True
                                , noutore = True
                                , ongaku = True
                                , insyokuari = True
                                , undou = True
                                , free = True
                                }
                        }
            in
            ( filtered newFilter model, Cmd.none )

        HideFilterControl ->
            ( { model | filterControlVisible = False }, Cmd.none )

        ChangeInput handler s ->
            ( handler model s, Cmd.none )

        LinkClicked _ ->
            nop model

        UrlChanged url ->
            ( { model | route = parseUrl url }, Cmd.none )

        ReceiveFileUrls fus ->
            ( { model | fileNameToUrl = Dict.fromList <| List.map (\fu -> ( fu.fileName, fu.url )) fus }
            , Cmd.none
            )

        Nop ->
            nop model


allLoaded : Model -> List Place -> List Kayoinoba -> ( Model, Cmd Msg )
allLoaded model places kayoinobas =
    let
        kl =
            KayoinobaList.new places kayoinobas
    in
    ( { model
        | kayoinobaList = kl
        , filtered = KayoinobaList.filtered model.filter kl
      }
    , Ports.requestFileUrls <| List.map Api.placeImageFileName places
    )


nop : Model -> ( Model, Cmd Msg )
nop m =
    ( m, Cmd.none )


fail : Model -> ( Model, Cmd Msg )
fail model =
    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model.map of
        Nothing ->
            { title = "マップ"
            , body =
                List.map H.toUnstyled <|
                    [ h1 [] [ text "Not Found" ]
                    ]
            }

        Just map ->
            { title = map.title
            , body = [ H.toUnstyled <| viewCore model map ]
            }


viewCore : Model -> Map -> Html Msg
viewCore model map =
    div [ css [ displayFlex, flexDirection column, Css.height (vh 100) ] ]
        [ Views.mapHeader map.title map.topInformation <| Views.c2c map.themeColor
        , viewKayoinobaList model
        , viewKayoinobaDetail model
        , viewFilterDialog model
        , H.fromUnstyled
            (GoogleMaps.init model.googleMapApiKey
                |> GoogleMaps.withZoom map.zoom
                |> GoogleMaps.withMapType GoogleMaps.roadmap
                |> GoogleMaps.withCenter map.centerLocation.latitude map.centerLocation.longitude
                |> GoogleMaps.withDefaultUIControls False
                -- |> GoogleMaps.withFitToMarkers True
                |> (GoogleMaps.withMarkers <|
                        List.map
                            (placeMarker model)
                            (KayoinobaList.places model.filtered)
                   )
                |> GoogleMaps.toHtml
            )
        ]


dialogZIndex : Style
dialogZIndex =
    zIndex (int 50)


placeImageSizeInList =
    80


stylePlaceImageInList : List Style
stylePlaceImageInList =
    [ Css.width (px placeImageSizeInList)
    , Css.height (px placeImageSizeInList)
    , border3 (px 1) solid (rgba 0 0 0 0.1)
    , display inlineBlock
    ]


viewNoImage : Html Msg
viewNoImage =
    div
        [ css <|
            textAlign center
                :: Css.color (rgb 150 140 150)
                :: backgroundColor (rgba 100 100 100 0.1)
                :: fontWeight bolder
                :: stylePlaceImageInList
        ]
        [ text "no image" ]


styleKayoinobaInfoBox : List Style
styleKayoinobaInfoBox =
    let
        ms =
            8
    in
    [ display inlineBlock
    , marginLeft (px ms)
    , verticalAlign top
    , maxWidth <| calc (pct 100) minus (px (placeImageSizeInList + ms + 2))
    ]


styleKayoinobaName : List Style
styleKayoinobaName =
    [ fontSize (pct 120)
    , fontWeight bold
    , color (rgb 50 50 50)
    , Styles.block
    ]


stylePlaceName : List Style
stylePlaceName =
    [ fontSize (pct 90)
    , fontWeight bold
    , color (rgb 150 30 30)
    , Styles.block
    ]


viewEmpty : Html msg
viewEmpty =
    div [ css [ display none ] ] []


styleDetailText : List Style
styleDetailText =
    [ fontWeight bold, color (rgb 100 100 100) ]


viewKayoinobaDetail : Model -> Html Msg
viewKayoinobaDetail model =
    case model.route of
        KayoinobaPage _ kayoinobaId ->
            let
                mKayoinoba =
                    KayoinobaList.kayoinobaFromId kayoinobaId model.kayoinobaList

                mPlace =
                    Maybe.andThen (\k -> KayoinobaList.placeForKayoinoba k model.kayoinobaList) mKayoinoba
            in
            case ( mKayoinoba, mPlace ) of
                ( Nothing, _ ) ->
                    viewEmpty

                ( _, Nothing ) ->
                    viewEmpty

                ( Just kayoinoba, Just place ) ->
                    Views.backdrop []
                        [ div
                            [ css
                                [ borderRadius (px 4)
                                , backgroundColor (rgb 255 255 255)
                                , border3 (px 1) solid (rgba 30 30 30 0.3)
                                , position absolute
                                , padding4 (px 10) (px 10) (px 10) (px 10)
                                , top (px 100)
                                , right (px 30)
                                , left (px 30)
                                , maxWidth (px 800)
                                , margin2 zero auto
                                , maxHeight <| calc (vh 100) minus (px 110)
                                , overflow scroll
                                ]
                            ]
                          <|
                            [ button
                                [ css [ Styles.borderNone, Styles.cursorPointer, position absolute, right (px 10) ]
                                , onClick HideKayoinobaDetail
                                ]
                                [ Views.icon WindowClose ]
                            , h1 [ css styleKayoinobaName ] [ text kayoinoba.name ]
                            , h2 [ css stylePlaceName ] [ text place.name ]
                            ]
                                ++ imgInDetail model.fileNameToUrl place
                                ++ [ div [ css styleDetailText ] [ text kayoinoba.summary ]
                                   , div [ css styleDetailText ] [ text <| Maybe.withDefault "価格設定なし" <| Maybe.map String.fromInt kayoinoba.price ]
                                   , div [ css styleDetailText ] [ text kayoinoba.target ]
                                   , div [ css styleDetailText ] [ text kayoinoba.contact ]
                                   , div [ css styleDetailText ] <|
                                        case kayoinoba.webSite of
                                            Nothing ->
                                                []

                                            Just url ->
                                                [ a [ href <| Api.fromUrl url, A.target "blank" ] [ text <| Api.fromUrl url ] ]
                                   ]
                        ]

        _ ->
            div [ css [ display none ] ] []


imgInDetail : Dict String String -> Place -> List (Html Msg)
imgInDetail fileNameToUrl place =
    case Dict.get (Api.placeImageFileName place) fileNameToUrl of
        Nothing ->
            []

        Just url ->
            [ img [ src url ] [] ]


viewKayoinobaInList : Dict String String -> KayoinobaList -> Kayoinoba -> Html Msg
viewKayoinobaInList fileNameToUrl ks kayoinoba =
    let
        ( placeImage, info ) =
            case KayoinobaList.placeForKayoinoba kayoinoba ks of
                Nothing ->
                    ( viewNoImage, [ span [] [ text kayoinoba.name ] ] )

                Just place ->
                    case Dict.get (Api.placeImageFileName place) fileNameToUrl of
                        Nothing ->
                            ( viewNoImage
                            , [ span [ css styleKayoinobaName ] [ text kayoinoba.name ]
                              , span [ css stylePlaceName ] [ text place.name ]
                              ]
                            )

                        Just url ->
                            ( img [ src url, css <| Styles.objectFitCover :: stylePlaceImageInList ] []
                            , [ span [ css styleKayoinobaName ] [ text kayoinoba.name ]
                              , span [ css stylePlaceName ] [ text place.name ]
                              ]
                            )
    in
    div
        [ css
            [ marginBottom (px 1)
            , hover [ backgroundColor (rgba 255 150 150 0.2) ]
            , Styles.cursorPointer
            ]
        , onClick <| ShowKayoinobaDetail kayoinoba
        ]
        [ placeImage
        , div [ css styleKayoinobaInfoBox ] info
        ]


widthSmartPhone =
    767


styleViewKayoinobaListOnPc : Style
styleViewKayoinobaListOnPc =
    Media.withMedia
        [ Media.only Media.screen
            [ Media.minWidth (px widthSmartPhone) ]
        ]
    <|
        Styles.list
            ++ [ display block
               , dialogZIndex
               , position absolute
               , top zero
               , right zero
               , Css.width (pct 30)
               , minHeight (Css.em 4)
               , maxHeight (vh 98)
               , padding (px 3)
               , backgroundColor (rgb 255 255 255)
               , border3 (px 1) solid (rgba 100 100 100 0.5)
               , overflow scroll
               ]


mediaQuerySmartPhone : List MediaQuery
mediaQuerySmartPhone =
    [ Media.only Media.screen
        [ Media.maxWidth (px widthSmartPhone) ]
    ]


styleViewKayoinobaListOnSmartPhone : Style
styleViewKayoinobaListOnSmartPhone =
    Media.withMedia
        mediaQuerySmartPhone
    <|
        Styles.list
            ++ [ display block
               , dialogZIndex
               , position fixed
               , right zero
               , left zero
               , bottom zero
               , minHeight (Css.em 1)
               , maxHeight (vh 25)
               , padding (px 3)
               , backgroundColor (rgb 255 255 255)
               , border3 (px 1) solid (rgba 100 100 100 0.5)
               , overflow scroll
               , displayFlex
               , flexWrap Css.wrap
               ]


viewKayoinobaList : Model -> Html Msg
viewKayoinobaList model =
    div [ css [ flexGrow (int 1), position relative ] ]
        [ Views.iconButton Search
            [ onClick ToggleFilterControl
            , css
                [ zIndex (int 10)
                , position absolute
                , top (px 4)
                , left (px 4)
                ]
            ]
        , ol
            [ css [ styleViewKayoinobaListOnPc, styleViewKayoinobaListOnSmartPhone ]
            ]
          <|
            List.map
                (\k ->
                    li
                        [ css
                            [ Media.withMedia mediaQuerySmartPhone <|
                                [ Css.width (pct 49)
                                , boxSizing borderBox
                                ]
                            ]
                        ]
                        [ viewKayoinobaInList model.fileNameToUrl
                            model.kayoinobaList
                            k
                        ]
                )
                (KayoinobaList.kayoinobas model.filtered)
        ]


viewFilterDialog : Model -> Html Msg
viewFilterDialog model =
    div [ css [ flexGrow (int 1), position relative ] ]
        [ div
            [ css <|
                [ borderRadius (px 4)
                , dialogZIndex
                , backgroundColor (rgb 255 255 255)
                , border3 (px 1) solid (rgba 30 30 30 0.3)
                , position absolute
                , padding4 (px 10) (px 10) (px 10) (px 10)
                , top (px 1)
                , left (px 50)
                , maxWidth (px 800)
                , margin2 zero auto
                , maxHeight (vh 90)
                , overflow scroll
                ]
                    ++ (if model.filterControlVisible then
                            []

                        else
                            [ display none ]
                       )
            ]
            [ h2 [] [ text "絞り込み" ]
            , Views.oneColumn <|
                Views.input (\src -> { src | value = model.filter.text, placeholder = "住所、施設名などを入力" }) (ChangeInput inputFilterText)
            , Views.oneColumn <|
                div []
                    [ button
                        [ type_ B.button
                        , Views.concatClass [ B.button, B.isSmall ]
                        , onClick ToggleAllPlaceCategories
                        ]
                        [ text "全て" ]
                    , viewCategoryIcon model.placeIconUrls Category1 (Maybe.map .category1Name model.map) model.filter.placeCategory.category1
                    , viewCategoryIcon model.placeIconUrls Category2 (Maybe.map .category2Name model.map) model.filter.placeCategory.category2
                    , viewCategoryIcon model.placeIconUrls Category3 (Maybe.map .category3Name model.map) model.filter.placeCategory.category3
                    ]
            , Views.oneColumn <|
                div []
                    [ button
                        [ type_ B.button
                        , Views.concatClass [ B.button, B.isSmall ]
                        , onClick ToggleAllKayoinobaAttributes
                        ]
                        [ text "全て" ]
                    , viewKayoinobaAttribute model.filter.kayoinobaAttributes.taisou model.kayoinobaAttributeIconUrls.taisou ToggleKATaisou
                    , viewKayoinobaAttribute model.filter.kayoinobaAttributes.noutore model.kayoinobaAttributeIconUrls.noutore ToggleKANoutore
                    , viewKayoinobaAttribute model.filter.kayoinobaAttributes.ongaku model.kayoinobaAttributeIconUrls.ongaku ToggleKAOngaku
                    , viewKayoinobaAttribute model.filter.kayoinobaAttributes.insyokuari model.kayoinobaAttributeIconUrls.insyokuari ToggleKAInsyokuari
                    , viewKayoinobaAttribute model.filter.kayoinobaAttributes.undou model.kayoinobaAttributeIconUrls.undou ToggleKAUndou
                    , viewKayoinobaAttribute model.filter.kayoinobaAttributes.free model.kayoinobaAttributeIconUrls.free ToggleKAFree
                    ]
            ]
        ]


viewKayoinobaAttribute : Bool -> String -> Msg -> Html Msg
viewKayoinobaAttribute selected iconUrl handler =
    div
        [ css
            [ Styles.selectableBackground selected
            , display inlineBlock
            , Css.width (px 64)
            , Css.height (px 64)
            , borderRadius (px 4)
            , margin (px 1)
            , padding (px 10)
            , cursor pointer
            ]
        , onClick handler
        ]
        [ div
            [ css
                [ backgroundImage (url iconUrl)
                , backgroundSize cover
                , Css.width (pct 100)
                , Css.height (pct 100)
                ]
            ]
            []
        ]


viewCategoryIcon :
    PlaceIconUrls
    -> PlaceCategory
    -> Maybe String
    -> Bool
    -> Html Msg
viewCategoryIcon urls kind label selected =
    div [ class B.buttons, css [ display inlineBlock, marginRight (px 2) ] ]
        [ button
            [ type_ B.button
            , class B.button
            , onClick (TogglePlaceCategoryFilter kind)
            , css
                [ Styles.selectableBackground selected
                , cursor pointer
                ]
            ]
            [ Views.categoryIcon32 urls kind
            , span [ css [ marginLeft (px 5) ] ] [ text <| Maybe.withDefault "" <| Maybe.map ((++) " ") label ]
            ]
        ]


placeMarker : Model -> Place -> Marker Msg
placeMarker model place =
    Marker.init place.location.latitude place.location.longitude
        |> Marker.withIcon
            (case place.category of
                Category1 ->
                    model.placeIconUrls.category1

                Category2 ->
                    model.placeIconUrls.category2

                Category3 ->
                    model.placeIconUrls.category3
            )
        |> Marker.withInfoWindow
            (List.map H.toUnstyled <|
                [ h1 [] [ text place.name ]
                , ul [ css Styles.list ] <|
                    List.map
                        (\k -> li [] [ viewKayoinobaInList model.fileNameToUrl model.filtered k ])
                        (KayoinobaList.kayoinobasForPlace place model.filtered)
                ]
            )


mapContainer : Html Msg
mapContainer =
    div [ id "map" ] []


inputFilterText : Model -> String -> Model
inputFilterText m s =
    let
        filter =
            m.filter

        newFilter =
            { filter | text = s }
    in
    { m | filter = newFilter, filtered = KayoinobaList.filtered newFilter m.kayoinobaList }
