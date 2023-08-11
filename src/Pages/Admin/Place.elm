module Pages.Admin.Place exposing (Model, Msg, page)

import Api
    exposing
        ( Map
        , MapId
        , PagingList
        , PagingParam
        , Place
        , ProtectedAccessParam
        , RemoteResource
        )
import Api.Enum.PlaceCategory as PlaceCategory exposing (PlaceCategory(..))
import Api.Scalar exposing (Id(..))
import Browser.Navigation as Navigation
import Bulma.Classes as B
import Css exposing (..)
import DataTable exposing (EditorVisible(..))
import Domain exposing (AppConfig, PlaceIconUrls)
import Effect exposing (Effect)
import File exposing (File)
import File.Select as Select
import Gen.Params.Admin.Place exposing (Params)
import Gen.Route
import Html.Styled exposing (Html, a, div, h2, img, p, span, text)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (..)
import List.Extra as List
import Page
import Ports
import RemoteData exposing (RemoteData(..))
import Request
import Route
import Shared
import Styles
import Tags exposing (..)
import Task
import View exposing (View)
import Views exposing (IconKind(..))


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


type alias SelectedFile =
    { file : Maybe File
    , url : String
    }


type alias Model =
    { appConfig : AppConfig
    , request : Request.With Params
    , pagingParam : PagingParam
    , maps : RemoteResource (List Map)
    , map : Maybe Map
    , places : RemoteResource (List Place)
    , editorVisible : EditorVisible
    , editorDialogVisible : Bool
    , editTarget : Place
    , placeImage : Maybe SelectedFile
    , placeImageErrorMessage : String
    , filterText : String
    , deleteDialogVisible : Bool
    , inputGoogleMapUrl : String
    , errorMessage : String
    }


getMapId : Model -> Maybe MapId
getMapId =
    .map >> Maybe.map .id


init : Shared.Model -> Request.With Params -> ( Model, Effect Msg )
init shared req =
    let
        pp =
            Api.startPagingParam 10
    in
    ( { appConfig = shared
      , request = req
      , pagingParam = pp
      , maps = NotAsked
      , map = Nothing
      , places = NotAsked
      , editorVisible = Hide
      , editorDialogVisible = False
      , editTarget = Api.defaultPlace
      , placeImage = Nothing
      , placeImageErrorMessage = " "
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
    | GotPlaces (RemoteResource (PagingList Place))
    | ChangeMapId String
    | OnLoadAddition
    | AppendPlaces (RemoteResource (PagingList Place))
    | ShowEditorForNew
    | ShowEditorForUpdate Place
    | ConfirmDelete Place
    | HideEditor
    | ChangeInput (String -> Model -> Model) String
    | ChangeInputPlaceCategory PlaceCategory
    | ClickSave
    | ClickDelete
    | ShowDetailPage Place
    | Saved (RemoteResource (Maybe Place))
    | ImageRequest
    | ImageSelected File
    | ImageLoaded String
    | GotImageUrl String


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
                    --全件取得完了. もしmap-idがURLに付与されていないなら、先頭地図を選択状態にする.
                    mapLoaded newModel

        GotMaps _ ->
            ( model, Cmd.none )

        GotPlaces (Success pPlaces) ->
            ( { model
                | places = Success pPlaces.items
                , pagingParam = { limit = model.pagingParam.limit, nextToken = pPlaces.nextToken }
              }
            , Cmd.none
            )

        GotPlaces _ ->
            fail model

        ChangeMapId v ->
            case model.maps of
                Success maps ->
                    case List.head <| List.filter (\map -> Api.fromId map.id == v) maps of
                        Nothing ->
                            ( model, Cmd.none )

                        Just map ->
                            ( { model | map = Just map }
                            , Cmd.batch
                                [ listPlaces model.appConfig model.pagingParam map.id
                                , Navigation.pushUrl model.request.key <| Route.adminPlaceHref <| Just map.id
                                ]
                            )

                _ ->
                    fail model

        OnLoadAddition ->
            ( model
            , case getMapId model of
                Nothing ->
                    Cmd.none

                Just mapId ->
                    Api.doPublicApi model.appConfig
                        (\param -> Api.listPlaces param model.pagingParam mapId AppendPlaces)
            )

        AppendPlaces mPlaces ->
            case ( model.places, mPlaces ) of
                ( Success currentPlaces, Success addition ) ->
                    ( { model
                        | places = Success <| currentPlaces ++ addition.items
                        , pagingParam = { limit = model.pagingParam.limit, nextToken = addition.nextToken }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | places = Api.mapRemoteResource .items mPlaces }, Cmd.none )

        ShowEditorForNew ->
            ( { model
                | editTarget = Api.defaultPlace
                , editorVisible = ShowForNew
              }
            , Cmd.none
            )

        ShowEditorForUpdate place ->
            ( { model
                | editTarget = place
                , editorVisible = ShowForUpdate
              }
            , if place.hasImage then
                Ports.requestFileUrl <| Api.placeImageFileName place

              else
                Cmd.none
            )

        ShowDetailPage place ->
            fail model

        ConfirmDelete place ->
            ( { model | deleteDialogVisible = True, editTarget = place }, Cmd.none )

        ChangeInput ope v ->
            ( ope v model, Cmd.none )

        ChangeInputPlaceCategory v ->
            let
                editTarget =
                    model.editTarget
            in
            ( { model | editTarget = { editTarget | category = v } }, Cmd.none )

        ClickSave ->
            savePlace model

        ClickDelete ->
            ( model, deletePlace model )

        Saved (Success (Just savedPlace)) ->
            case model.placeImage of
                Nothing ->
                    finishSave model

                Just f ->
                    case f.file of
                        Nothing ->
                            finishSave model

                        Just file ->
                            ( hideEditor model
                            , Ports.uploadToStorage
                                { fileName = Api.placeImageFileName savedPlace
                                , dataUrl = f.url
                                , contentType = File.mime file
                                }
                            )

        Saved _ ->
            fail model

        HideEditor ->
            ( hideEditor model
            , Cmd.none
            )

        ImageRequest ->
            ( model, Select.file [ "image/*" ] ImageSelected )

        ImageSelected file ->
            if File.size file > 1024 * 1024 then
                ( { model | placeImageErrorMessage = "画像ファイルのサイズは1MB以内にして下さい" }, Cmd.none )

            else
                let
                    et =
                        model.editTarget
                in
                ( { model
                    | placeImageErrorMessage = " "
                    , placeImage = Just { file = Just file, url = "" }
                    , editTarget = { et | hasImage = True }
                  }
                , Task.perform ImageLoaded <| File.toUrl file
                )

        ImageLoaded url ->
            case model.placeImage of
                Nothing ->
                    fail model

                Just file ->
                    ( { model | placeImage = Just { file | url = url } }, Cmd.none )

        GotImageUrl url ->
            ( { model | placeImage = Just { file = Nothing, url = url } }, Cmd.none )


listMaps : AppConfig -> String -> Cmd Msg
listMaps appConfig nextToken =
    Api.doPublicApi appConfig
        (\param ->
            Api.listMaps param { nextToken = Just nextToken, limit = mapFetchCount } GotMaps
        )


mapFetchCount : number
mapFetchCount =
    50


listPlaces : AppConfig -> PagingParam -> MapId -> Cmd Msg
listPlaces appConfig pagingParam mapId =
    Api.doPublicApi appConfig (\param -> Api.listPlaces param pagingParam mapId GotPlaces)


mapLoaded : Model -> ( Model, Cmd Msg )
mapLoaded model =
    case ( getMapId model, model.maps ) of
        ( Nothing, Success (map :: _) ) ->
            ( { model | map = Just map }
            , Cmd.batch
                [ Navigation.pushUrl model.request.key <| Route.adminPlaceHref <| Just map.id
                , listPlaces model.appConfig model.pagingParam map.id
                ]
            )

        ( Just mapId, Success maps ) ->
            case List.find (\map -> mapId == map.id) maps of
                Nothing ->
                    ( model, Cmd.none )

                Just map ->
                    ( { model | map = Just map }
                    , listPlaces model.appConfig model.pagingParam mapId
                    )

        _ ->
            ( model, Cmd.none )


fail : Model -> ( Model, Cmd Msg )
fail m =
    ( m, Debug.log "fail" Cmd.none )


hideEditor : Model -> Model
hideEditor model =
    { model
        | editorVisible = Hide
        , deleteDialogVisible = False
        , inputGoogleMapUrl = ""
        , placeImage = Nothing
        , errorMessage = ""
    }


finishSave : Model -> ( Model, Cmd Msg )
finishSave model =
    ( hideEditor model
    , case getMapId model of
        Nothing ->
            Cmd.none

        Just mapId ->
            listPlaces model.appConfig model.pagingParam mapId
    )


savePlace : Model -> ( Model, Cmd Msg )
savePlace model =
    case model.editorVisible of
        ShowForNew ->
            ( model
            , doProtectedApi model (\param mapId -> Api.createPlace param mapId model.editTarget Saved)
            )

        ShowForUpdate ->
            ( model
            , doProtectedApi model
                (\param mapId ->
                    Api.updatePlace param mapId model.editTarget Saved
                )
            )

        _ ->
            ( model, Cmd.none )


deletePlace : Model -> Cmd Msg
deletePlace model =
    doProtectedApi model
        (\param _ ->
            Api.deletePlace param
                model.editTarget.id
                Saved
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveFileUrl GotImageUrl



-- VIEW


title : String
title =
    "場所の管理"


view : Model -> View Msg
view model =
    { title = title
    , body = Views.layout OnSignOut (Maybe.map .id model.map) Gen.Route.Admin__Place <| viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    case model.maps of
        Success [] ->
            [ h1 [] [ text title ]
            , h2 []
                [ text "マップが登録されていません。"
                , a [ href Route.adminMapHref ] [ text "こちら" ]
                , text "から登録してください。"
                ]
            ]

        Success maps ->
            [ h1 [] [ text title ]
            , Views.select
                { value = model.map
                , values = maps
                , valueToString = \m -> Api.fromId <| m.id
                , valueToLabel = \m -> m.name
                , handler = ChangeMapId
                , attributes = []
                }
            , viewPlaces model
            ]

        _ ->
            []


viewPlaces : Model -> Html Msg
viewPlaces model =
    DataTable.table
        (tableParam model)
    <|
        RemoteData.withDefault [] model.places


tableParam : Model -> DataTable.Param Place Msg
tableParam model =
    { columns =
        [ { widthClass = B.is3
          , label = "名前"
          , html =
                \place ->
                    div [] [ span [] [ text place.name ] ]
          }
        , { widthClass = B.is1
          , label = ""
          , html =
                \place -> Views.categoryIcon32 model.appConfig.placeIconUrls place.category
          }
        , { widthClass = B.is4
          , label = "住所"
          , html =
                \place ->
                    div [] [ span [] [ text place.address ] ]
          }
        , { widthClass = B.is1
          , label = "緯度"
          , html =
                \place ->
                    div [] [ span [] [ text <| String.fromFloat place.location.latitude ] ]
          }
        , { widthClass = B.is1
          , label = "経度"
          , html =
                \place ->
                    div [] [ span [] [ text <| String.fromFloat place.location.longitude ] ]
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
        long =
            model.editTarget.location.longitude

        lat =
            model.editTarget.location.latitude
    in
    String.trim model.editTarget.name
        /= ""
        && (-180 <= long && long <= 180)
        && (-90 <= lat && lat <= 90)


viewPlaceImage : Model -> Html Msg
viewPlaceImage model =
    div
        [ css
            [ position relative
            , Css.minHeight (px 196)
            ]
        ]
    <|
        Views.iconButton Image
            [ onClick ImageRequest
            , css
                [ display block
                , position absolute
                , top (px 2)
                , left (px 2)
                , zIndex (int 100)
                ]
            ]
            :: (case model.placeImage of
                    Nothing ->
                        []

                    Just file ->
                        [ img
                            [ src file.url
                            , css
                                [ position absolute
                                , top (px 2)
                                , left (px 2)
                                , maxHeight (calc (pct 100) minus (px 4))
                                , maxWidth (calc (pct 100) minus (px 4))
                                , border3 (px 1) solid (rgba 150 150 150 0.3)
                                ]
                            ]
                            []
                        ]
               )


viewEditor : Model -> List (Html Msg)
viewEditor model =
    case model of
        { editTarget } ->
            [ p [ css Styles.errorMessage ] [ text model.errorMessage ]
            , Views.oneColumn <|
                div []
                    [ Views.input (\arg -> { arg | label = "場所名", value = editTarget.name }) (ChangeInput inputPlaceName)
                    , a [ Attributes.target "google-map", href <| "https://www.google.co.jp/maps/search/" ++ editTarget.name ++ "/" ] [ text "Google Mapで場所検索" ]
                    ]
            , Views.oneColumn <|
                Views.input (\arg -> { arg | label = "住所", value = editTarget.address }) (ChangeInput inputPlaceAddress)
            , Views.oneColumn <|
                div []
                    [ label [] [ text "施設の画像" ]
                    , p [ css <| Styles.errorMessage ++ [ minHeight (em 1) ] ] [ text model.placeImageErrorMessage ]
                    , viewPlaceImage model
                    ]
            ]
                ++ Views.location
                    { lat = editTarget.location.latitude
                    , lon = editTarget.location.longitude
                    , googleMapUrl = model.inputGoogleMapUrl
                    , latHandler = ChangeInput inputPlaceLatitude
                    , lonHandler = ChangeInput inputPlaceLongitude
                    , googleMapUrlHandler = ChangeInput inputGoogleMapUrl
                    }
                ++ [ Views.oneColumn <|
                        div []
                            [ label [] [ text "地図上のアイコン" ]
                            , selectPlaceCategory model.appConfig.placeIconUrls editTarget.category ChangeInputPlaceCategory
                            ]
                   ]


selectPlaceCategory : PlaceIconUrls -> PlaceCategory -> (PlaceCategory -> msg) -> Html msg
selectPlaceCategory placeIconUrls selected handler =
    let
        bt =
            \kind ->
                button
                    [ type_ B.button
                    , onClick <| handler kind
                    , class B.button
                    , css
                        [ Styles.selectableBackground (kind == selected)
                        , cursor pointer
                        ]
                    ]
                    [ Views.categoryIcon32 placeIconUrls kind ]
    in
    div [ class B.buttons, class B.hasAddons ] <| List.map bt PlaceCategory.list


doProtectedApi : Model -> (ProtectedAccessParam -> MapId -> Cmd Msg) -> Cmd Msg
doProtectedApi model operation =
    case model.map of
        Nothing ->
            Cmd.none

        Just map ->
            Api.doProtectedApi model.appConfig (\param -> operation param map.id)


inputGoogleMapUrl : String -> Model -> Model
inputGoogleMapUrl v m =
    case Views.extractLatLonFromGoogleMapUrl v of
        Nothing ->
            { m | inputGoogleMapUrl = v }

        Just ( lat, lon ) ->
            let
                target =
                    m.editTarget
            in
            { m
                | editTarget = { target | location = { latitude = lat, longitude = lon } }
                , inputGoogleMapUrl = v
            }


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


inputPlaceName : String -> Model -> Model
inputPlaceName v m =
    let
        target =
            m.editTarget
    in
    { m | editTarget = { target | name = v } }


inputPlaceAddress : String -> Model -> Model
inputPlaceAddress v m =
    let
        target =
            m.editTarget
    in
    { m | editTarget = { target | address = v } }


inputPlaceLongitude : String -> Model -> Model
inputPlaceLongitude v model =
    case String.toFloat v of
        Nothing ->
            model

        Just f ->
            let
                target =
                    model.editTarget

                location =
                    target.location
            in
            { model | editTarget = { target | location = { location | longitude = f } } }


inputPlaceLatitude : String -> Model -> Model
inputPlaceLatitude v model =
    case String.toFloat v of
        Nothing ->
            model

        Just f ->
            let
                target =
                    model.editTarget

                location =
                    target.location
            in
            { model | editTarget = { target | location = { location | latitude = f } } }


inputFilterText : String -> Model -> Model
inputFilterText v m =
    { m | filterText = v }
