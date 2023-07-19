module Pages.Admin.Kayoinoba exposing (Model, Msg, page)

import Api
    exposing
        ( Kayoinoba
        , Map
        , MapId
        , PagingList
        , PagingParam
        , PlaceName
        , RemoteResource
        )
import Api.Scalar exposing (Awsurl(..), Id(..))
import Browser.Navigation as Navigation
import Bulma.Classes as B
import Css exposing (..)
import DataTable exposing (EditorVisible(..))
import Dict
import Domain exposing (AppConfig)
import Effect exposing (Effect)
import Gen.Params.Admin.Kayoinoba exposing (Params)
import Html.Styled exposing (Html, a, div, h1, p, span, text)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (..)
import List.Extra as List
import Page
import RemoteData exposing (RemoteData(..))
import Request
import Route
import Shared
import Styles
import View exposing (View)
import Views exposing (IconKind(..), label)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.protected.advanced <|
        \_ ->
            { init = init shared req
            , update = update
            , view = view
            , subscriptions = subscriptions
            }



-- INIT


type alias Model =
    { appConfig : AppConfig
    , request : Request.With Params
    , pagingParam : PagingParam
    , maps : RemoteResource (List Map)
    , map : Maybe Map
    , places : List PlaceName
    , kayoinobas : RemoteResource (List Kayoinoba)
    , editorVisible : EditorVisible
    , editorDialogVisible : Bool
    , editTarget : Kayoinoba
    , filterText : String
    , deleteDialogVisible : Bool
    , inputGoogleMapUrl : String
    , errorMessage : String
    }


getMapId : Model -> Maybe MapId
getMapId model =
    Maybe.map Id <| Dict.get "map-id" model.request.query


init : Shared.Model -> Request.With Params -> ( Model, Effect Msg )
init shared req =
    ( { appConfig = shared
      , request = req
      , pagingParam = Api.startPagingParam 10
      , maps = NotAsked
      , map = Nothing
      , places = []
      , kayoinobas = NotAsked
      , editorVisible = Hide
      , editorDialogVisible = False
      , editTarget = Api.defaultKayoinoba
      , filterText = ""
      , deleteDialogVisible = False
      , inputGoogleMapUrl = ""
      , errorMessage = ""
      }
    , Effect.fromCmd <|
        Api.doPublicApi shared
            (\param ->
                Api.listMaps param (Api.startPagingParam mapFetchCount) GotMaps
            )
    )



-- UPDATE


type Msg
    = OnSignOut
    | GotMaps (RemoteResource (PagingList Map))
    | GotPlaces EditorVisible (RemoteResource (PagingList PlaceName))
    | GotKayoinobas (RemoteResource (PagingList Kayoinoba))
    | ChangeMapId String
    | OnLoadAddition
    | AppendKayoinobas (RemoteResource (PagingList Kayoinoba))
    | ShowEditorForNew
    | ShowEditorForUpdate Kayoinoba
    | ConfirmDelete Kayoinoba
    | HideEditor
    | ChangeInput (String -> Model -> Model) String
    | ClickSave
    | ClickDelete
    | ShowDetailPage Kayoinoba
    | Saved (RemoteResource (Maybe Kayoinoba))
    | ReloadPlaces
    | ToggleTaisou
    | ToggleNoutore
    | ToggleOngaku
    | ToggleInsyokuari
    | ToggleUndou
    | ToggleFree


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    let
        ( m, c ) =
            updateCore msg model
    in
    ( m, Effect.fromCmd c )


updateCore : Msg -> Model -> ( Model, Cmd Msg )
updateCore msg model =
    case msg of
        OnSignOut ->
            ( model, Shared.signOut () )

        GotMaps (Success pMaps) ->
            let
                newModel =
                    { model
                        | maps =
                            case model.maps of
                                Success current ->
                                    Success <| current ++ pMaps.items

                                _ ->
                                    Success pMaps.items
                    }
            in
            case pMaps.nextToken of
                Just token ->
                    --まだサーバに地図情報が残っているため、追加取得する
                    ( newModel
                    , listMaps model.appConfig token
                    )

                Nothing ->
                    mapLoaded newModel

        GotMaps _ ->
            ( model, Cmd.none )

        GotPlaces editorVisible (Success pPlaces) ->
            case pPlaces.nextToken of
                Nothing ->
                    let
                        newModel =
                            { model | editorVisible = editorVisible, places = model.places ++ pPlaces.items }
                    in
                    case ( model.editTarget.kayoinobaPlaceId, model.places ) of
                        ( Id "", p :: _ ) ->
                            ( replaceEditTarget newModel (\et -> { et | kayoinobaPlaceId = p.id })
                            , Cmd.none
                            )

                        _ ->
                            ( newModel, Cmd.none )

                Just nextToken ->
                    ( { model | places = model.places ++ pPlaces.items }
                    , listPlaceNames
                        model.appConfig
                        { nextToken = Just nextToken, limit = placeFetchCount }
                        (getMapId model)
                        (GotPlaces ShowForNew)
                    )

        GotPlaces _ _ ->
            ( model, Cmd.none )

        GotKayoinobas (Success pKayoinoba) ->
            ( { model
                | kayoinobas = Success pKayoinoba.items
                , pagingParam = { limit = model.pagingParam.limit, nextToken = pKayoinoba.nextToken }
              }
            , Cmd.none
            )

        GotKayoinobas _ ->
            ( model, Cmd.none )

        ChangeMapId v ->
            case model.maps of
                Success maps ->
                    case List.head <| List.filter (\map -> Api.fromId map.id == v) maps of
                        Nothing ->
                            ( model, Cmd.none )

                        Just map ->
                            ( { model | map = Just map }
                            , Cmd.batch
                                [ listKayoinobas model.appConfig model.pagingParam map.id
                                , Navigation.pushUrl model.request.key <| Route.adminKayoinobaHref <| Just map.id
                                ]
                            )

                _ ->
                    ( model, Cmd.none )

        OnLoadAddition ->
            ( model
            , case getMapId model of
                Nothing ->
                    Cmd.none

                Just mapId ->
                    Api.doPublicApi model.appConfig
                        (\param -> Api.listKayoinobas param model.pagingParam mapId AppendKayoinobas)
            )

        AppendKayoinobas mKayoinobas ->
            case ( model.kayoinobas, mKayoinobas ) of
                ( Success currentKayoinobas, Success addition ) ->
                    ( { model
                        | kayoinobas = Success <| currentKayoinobas ++ addition.items
                        , pagingParam = { limit = model.pagingParam.limit, nextToken = addition.nextToken }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | kayoinobas = Api.mapRemoteResource .items mKayoinobas }, Cmd.none )

        ShowEditorForNew ->
            case getMapId model of
                Nothing ->
                    ( model, Cmd.none )

                Just mapId ->
                    let
                        dk =
                            Api.defaultKayoinoba
                    in
                    ( { model
                        | editTarget = { dk | mapIdForSearch = mapId }
                      }
                    , listPlaceNames
                        model.appConfig
                        (Api.startPagingParam placeFetchCount)
                        (getMapId model)
                        (GotPlaces ShowForNew)
                    )

        ShowEditorForUpdate kayoinoba ->
            ( { model
                | editTarget = kayoinoba
                , editorVisible = ShowForUpdate
              }
            , listPlaceNames
                model.appConfig
                (Api.startPagingParam placeFetchCount)
                (getMapId model)
                (GotPlaces ShowForUpdate)
            )

        ShowDetailPage kayoinoba ->
            ( model, Navigation.pushUrl model.request.key <| Route.adminKayoinobaHref <| Just kayoinoba.id )

        ConfirmDelete kayoinoba ->
            ( { model | deleteDialogVisible = True, editTarget = kayoinoba }, Cmd.none )

        ChangeInput ope v ->
            ( ope v model, Cmd.none )

        ClickSave ->
            case model.editorVisible of
                ShowForNew ->
                    ( model
                    , Api.doProtectedApi model.appConfig (\param -> Api.createKayoinoba param model.editTarget Saved)
                    )

                ShowForUpdate ->
                    ( model
                    , Api.doProtectedApi model.appConfig (\param -> Api.updateKayoinoba param model.editTarget Saved)
                    )

                _ ->
                    ( model, Cmd.none )

        ClickDelete ->
            ( model, deleteKayoinoba model )

        Saved _ ->
            ( { model
                | editorVisible = Hide
                , deleteDialogVisible = False
                , places = []
                , inputGoogleMapUrl = ""
                , errorMessage = ""
              }
            , case getMapId model of
                Nothing ->
                    Cmd.none

                Just mapId ->
                    listKayoinobas model.appConfig model.pagingParam mapId
            )

        HideEditor ->
            ( { model
                | editorVisible = Hide
                , deleteDialogVisible = False
                , errorMessage = ""
              }
            , Cmd.none
            )

        ReloadPlaces ->
            ( { model | places = [] }
            , listPlaceNames
                model.appConfig
                (Api.startPagingParam placeFetchCount)
                (getMapId model)
                (GotPlaces model.editorVisible)
            )

        ToggleTaisou ->
            let
                et =
                    model.editTarget

                ka =
                    et.attributes
            in
            ( { model | editTarget = { et | attributes = { ka | taisou = not ka.taisou } } }, Cmd.none )

        ToggleNoutore ->
            let
                et =
                    model.editTarget

                ka =
                    et.attributes
            in
            ( { model | editTarget = { et | attributes = { ka | noutore = not ka.noutore } } }, Cmd.none )

        ToggleOngaku ->
            let
                et =
                    model.editTarget

                ka =
                    et.attributes
            in
            ( { model | editTarget = { et | attributes = { ka | ongaku = not ka.ongaku } } }, Cmd.none )

        ToggleInsyokuari ->
            let
                et =
                    model.editTarget

                ka =
                    et.attributes
            in
            ( { model | editTarget = { et | attributes = { ka | insyokuari = not ka.insyokuari } } }, Cmd.none )

        ToggleUndou ->
            let
                et =
                    model.editTarget

                ka =
                    et.attributes
            in
            ( { model | editTarget = { et | attributes = { ka | undou = not ka.undou } } }, Cmd.none )

        ToggleFree ->
            let
                et =
                    model.editTarget

                ka =
                    et.attributes
            in
            ( { model | editTarget = { et | attributes = { ka | free = not ka.free } } }, Cmd.none )


deleteKayoinoba : Model -> Cmd Msg
deleteKayoinoba model =
    Api.doProtectedApi model.appConfig
        (\param ->
            Api.deleteKayoinoba param
                model.editTarget.id
                Saved
        )


listMaps : AppConfig -> String -> Cmd Msg
listMaps apiParam nextToken =
    Api.doPublicApi apiParam
        (\param ->
            Api.listMaps param { nextToken = Just nextToken, limit = mapFetchCount } GotMaps
        )


mapFetchCount : Int
mapFetchCount =
    50


placeFetchCount : Int
placeFetchCount =
    50


listKayoinobas : AppConfig -> PagingParam -> MapId -> Cmd Msg
listKayoinobas appConfig pagingParam mapId =
    Api.doPublicApi appConfig (\param -> Api.listKayoinobas param pagingParam mapId GotKayoinobas)


listPlaceNames :
    AppConfig
    -> PagingParam
    -> Maybe MapId
    -> (RemoteResource (PagingList PlaceName) -> Msg)
    -> Cmd Msg
listPlaceNames appConfig pagingParam mMapId handler =
    case mMapId of
        Nothing ->
            Cmd.none

        Just mapId ->
            Api.doPublicApi appConfig (\param -> Api.listPlaceNames param pagingParam mapId handler)


mapLoaded : Model -> ( Model, Cmd Msg )
mapLoaded model =
    case ( getMapId model, model.maps ) of
        ( Nothing, Success (map :: _) ) ->
            ( { model | map = Just map }
            , Cmd.batch
                [ Navigation.pushUrl model.request.key <| Route.adminKayoinobaHref <| Just map.id
                , listKayoinobas model.appConfig model.pagingParam map.id
                ]
            )

        ( Just mapId, Success maps ) ->
            case List.find (\map -> mapId == map.id) maps of
                Nothing ->
                    ( model, Cmd.none )

                Just map ->
                    ( { model | map = Just map }
                    , listKayoinobas model.appConfig model.pagingParam map.id
                    )

        _ ->
            ( model, Cmd.none )


replaceEditTarget : Model -> (Kayoinoba -> Kayoinoba) -> Model
replaceEditTarget model operation =
    let
        et =
            model.editTarget
    in
    { model | editTarget = operation et }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "地図[" ++ (Maybe.withDefault "" <| Maybe.map (\map -> map.name) model.map) ++ "]詳細"
    , body =
        Views.layout OnSignOut (getMapId model) <|
            case model.maps of
                Success [] ->
                    [ h1 []
                        [ text "マップが登録されていません。"
                        , a [ href Route.adminMapHref ] [ text "こちら" ]
                        , text "から登録してください。"
                        ]
                    ]

                Success maps ->
                    [ Views.select
                        { value = model.map
                        , values = maps
                        , valueToString = \m -> Api.fromId <| m.id
                        , valueToLabel = \m -> m.name
                        , handler = ChangeMapId
                        , attributes = []
                        }
                    , DataTable.table (tableParam model) <| RemoteData.withDefault [] model.kayoinobas
                    ]

                _ ->
                    []
    }


tableParam : Model -> DataTable.Param Kayoinoba Msg
tableParam model =
    { columns =
        [ { widthClass = B.is3
          , label = "名前"
          , html =
                \kayoinoba ->
                    div [] [ span [] [ text kayoinoba.name ] ]
          }
        ]
    , pagingParam = model.pagingParam
    , fetchCountList = [ 10, 20, 50, 100 ]
    , changeFetchCount = ChangeInput inputFetchCount
    , changeFilterText = ChangeInput inputFilterText
    , confirmDelete = ConfirmDelete
    , deleteCancel = HideEditor
    , deleteDialogVisible = model.deleteDialogVisible
    , deleteDialog = \_ -> [ p [] [ text "本当に削除していいですか？" ] ]
    , deleteOk = ClickDelete
    , editor = \_ -> viewEditor model
    , checkInputForNew = checkInputForNew model
    , clickSave = ClickSave
    , hideEditor = HideEditor
    , editorVisible = model.editorVisible
    , filterText = model.filterText
    , onLoadAddition = OnLoadAddition
    , showDetailPage = ShowDetailPage
    , showEditorForNew = ShowEditorForNew
    , showEditorForUpdate = ShowEditorForUpdate
    }


checkInputForNew : Model -> Bool
checkInputForNew model =
    let
        et =
            model.editTarget
    in
    String.trim et.name
        /= ""
        && String.trim (Api.fromId et.kayoinobaPlaceId)
        /= ""
        && String.trim et.target
        /= ""
        && String.trim et.contact
        /= ""
        && not
            (not et.attributes.taisou
                -- 属性が１つは設定されていないといけない
                && not et.attributes.noutore
                && not et.attributes.ongaku
                && not et.attributes.insyokuari
                && not et.attributes.undou
                && not et.attributes.free
            )


viewEditor : Model -> List (Html Msg)
viewEditor model =
    [ p [ css [ color (rgb 255 0 0) ] ] [ text model.errorMessage ]
    , Views.oneColumn <|
        Views.input
            (\arg -> { arg | label = "名称", value = model.editTarget.name })
            (ChangeInput inputKayoinobaName)
    , Views.oneColumn <|
        Views.input
            (\arg -> { arg | label = "読み仮名(検索時に使用)", value = model.editTarget.yomigana })
            (ChangeInput inputKayoinobaYomigana)
    , Views.oneColumn <|
        div []
            [ label [] [ text "開催場所" ]
            , div []
                [ a [ href <| Route.adminPlaceHref <| getMapId model, Attributes.target "blank_" ]
                    [ text "ここにない開催場所を使いたい場合はこちらから登録をお願いします。" ]
                ]
            , div [ class B.columns, css [ alignItems center ] ]
                [ div [ class B.column, class B.is11 ]
                    [ Views.select
                        { value = List.find (\elem -> elem.id == model.editTarget.kayoinobaPlaceId) model.places
                        , values = model.places
                        , valueToString = \m -> Api.fromId m.id
                        , valueToLabel = \m -> m.name
                        , handler = ChangeInput inputKayoinobaPlaceId
                        , attributes = []
                        }
                    ]
                , Views.iconButton Redo [ onClick ReloadPlaces ]
                ]
            ]
    , Views.oneColumn <|
        Views.textArea
            (\src ->
                { src
                    | value = model.editTarget.summary
                    , label = "概要"
                }
            )
            (ChangeInput inputKayoinobaSummary)
    , Views.oneColumn <|
        Views.input
            (\arg ->
                { arg
                    | label = "費用"
                    , type_ = "number"
                    , value = Maybe.withDefault "" <| Maybe.map String.fromInt model.editTarget.price
                }
            )
            (ChangeInput inputKayoinobaPrice)
    , Views.oneColumn <|
        Views.input
            (\arg ->
                { arg
                    | label = "対象"
                    , value = model.editTarget.target
                }
            )
            (ChangeInput inputKayoinobaTarget)
    , Views.oneColumn <|
        Views.input
            (\arg ->
                { arg
                    | label = "連絡先"
                    , value = model.editTarget.contact
                }
            )
            (ChangeInput inputKayoinobaContact)
    , Views.oneColumn <|
        div []
            [ label [] [ text "通いの場のURL" ]
            , div [ class B.columns, css [ alignItems center ] ]
                [ div [ class B.column, class B.is10 ]
                    [ Views.input
                        (\arg ->
                            { arg
                                | value = Maybe.withDefault "" <| Maybe.map Api.fromUrl model.editTarget.webSite
                            }
                        )
                        (ChangeInput inputKayoinobaWebSite)
                    ]
                , div [ class B.column ] [ viewUrlValidationLink model.editTarget.webSite ]
                ]
            ]
    , div [ class B.columns, css [ marginLeft zero, marginTop (rem 1) ] ]
        [ viewKayoinobaAttribute model.editTarget.attributes.taisou model.appConfig.kayoinobaAttributeIconUrls.taisou ToggleTaisou
        , viewKayoinobaAttribute model.editTarget.attributes.noutore model.appConfig.kayoinobaAttributeIconUrls.noutore ToggleNoutore
        , viewKayoinobaAttribute model.editTarget.attributes.ongaku model.appConfig.kayoinobaAttributeIconUrls.ongaku ToggleOngaku
        , viewKayoinobaAttribute model.editTarget.attributes.insyokuari model.appConfig.kayoinobaAttributeIconUrls.insyokuari ToggleInsyokuari
        , viewKayoinobaAttribute model.editTarget.attributes.undou model.appConfig.kayoinobaAttributeIconUrls.undou ToggleUndou
        , viewKayoinobaAttribute model.editTarget.attributes.free model.appConfig.kayoinobaAttributeIconUrls.free ToggleFree
        ]
    ]


viewUrlValidationLink : Maybe Awsurl -> Html Msg
viewUrlValidationLink webSite =
    let
        disable =
            css [ color (rgb 100 100 100), pointerEvents none ]

        style =
            case webSite of
                Nothing ->
                    [ disable ]

                Just (Awsurl url) ->
                    if String.startsWith "http://" url || String.startsWith "https://" url then
                        []

                    else
                        [ disable ]
    in
    a
        ([ href <| Maybe.withDefault "" <| Maybe.map Api.fromUrl webSite
         , Attributes.target "blank_"
         ]
            ++ style
        )
        [ Views.icon ExternalLinkAlt, text "URL確認" ]


viewKayoinobaAttribute : Bool -> String -> Msg -> Html Msg
viewKayoinobaAttribute selected iconUrl handler =
    div
        [ css
            [ Styles.selectableBackground selected
            , Css.width (px 120)
            , Css.height (px 120)
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


inputFetchCount : String -> Model -> Model
inputFetchCount v m =
    case String.toInt v of
        Nothing ->
            m

        Just i ->
            let
                pp =
                    m.pagingParam
            in
            { m | pagingParam = { pp | limit = i } }


inputKayoinobaName : String -> Model -> Model
inputKayoinobaName v m =
    let
        target =
            m.editTarget
    in
    { m | editTarget = { target | name = v } }


inputKayoinobaSummary : String -> Model -> Model
inputKayoinobaSummary v m =
    let
        target =
            m.editTarget
    in
    { m | editTarget = { target | summary = v } }


inputKayoinobaYomigana : String -> Model -> Model
inputKayoinobaYomigana v m =
    let
        target =
            m.editTarget
    in
    { m | editTarget = { target | yomigana = v } }


inputKayoinobaPrice : String -> Model -> Model
inputKayoinobaPrice v m =
    let
        target =
            m.editTarget
    in
    case v of
        "" ->
            { m | editTarget = { target | price = Nothing } }

        _ ->
            case String.toInt v of
                Nothing ->
                    m

                Just i ->
                    { m | editTarget = { target | price = Just i } }


inputFilterText : String -> Model -> Model
inputFilterText v m =
    { m | filterText = v }


inputKayoinobaPlaceId : String -> Model -> Model
inputKayoinobaPlaceId v m =
    let
        target =
            m.editTarget
    in
    { m | editTarget = { target | kayoinobaPlaceId = Id v } }


inputKayoinobaTarget : String -> Model -> Model
inputKayoinobaTarget v m =
    let
        target =
            m.editTarget
    in
    { m | editTarget = { target | target = v } }


inputKayoinobaContact : String -> Model -> Model
inputKayoinobaContact v m =
    let
        target =
            m.editTarget
    in
    { m | editTarget = { target | contact = v } }


inputKayoinobaWebSite : String -> Model -> Model
inputKayoinobaWebSite v m =
    let
        target =
            m.editTarget
    in
    { m | editTarget = { target | webSite = Just <| Awsurl v } }
