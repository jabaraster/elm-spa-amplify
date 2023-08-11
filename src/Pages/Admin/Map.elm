module Pages.Admin.Map exposing (Model, Msg, page)

import Api
    exposing
        ( Map
        , PagingList
        , PagingParam
        , RemoteResource
        )
import Api.Scalar exposing (Id(..))
import Bulma.Classes as B
import ColorPicker
import Css exposing (..)
import DataTable exposing (EditorVisible(..))
import Domain exposing (AppConfig)
import Effect exposing (Effect)
import Gen.Route
import Html
import Html.Styled exposing (Html, a, br, div, hr, input, p, span, text)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Page
import RemoteData exposing (RemoteData(..))
import Request exposing (Request)
import Shared
import Tags exposing (..)
import View exposing (View)
import Views


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.protected.advanced <|
        \_ ->
            { init = init shared
            , update = update
            , view = view
            , subscriptions = \_ -> Sub.none
            }



-- INIT


type alias Model =
    { appConfig : AppConfig
    , pagingParam : PagingParam
    , maps : RemoteResource (List Map)
    , filterText : String
    , editorVisible : EditorVisible
    , deleteDialogVisible : Bool
    , editTarget : Map
    , inputGoogleMapUrl : String
    , errorMessage : String
    , colorPicker : ColorPicker.State
    }


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    let
        pagingParam =
            Api.startPagingParam 5
    in
    ( { appConfig = shared
      , pagingParam = pagingParam
      , maps = NotAsked
      , filterText = ""
      , editorVisible = Hide
      , deleteDialogVisible = False
      , editTarget = Api.defaultMap
      , inputGoogleMapUrl = ""
      , errorMessage = ""
      , colorPicker = ColorPicker.empty
      }
    , Effect.fromCmd <|
        Api.doPublicApi shared (\param -> Api.listMaps param pagingParam GotMaps)
    )



-- UPDATE


type Msg
    = OnSignOut
    | GotMaps (RemoteResource (PagingList Map))
    | AppendMaps (RemoteResource (PagingList Map))
    | ShowEditorForNew
    | ShowEditorForUpdate Map
    | ShowDetailPage Map
    | ChangeInput (String -> Model -> Model) String
    | CheckIdDuplicate (RemoteResource (Maybe ()))
    | ClickDelete
    | ConfirmDelete Map
    | HideEditor
    | Saved (RemoteResource (Maybe Map))
    | ClickSave
    | OnLoadAddition
    | ColorPickerMsg ColorPicker.Msg


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        OnSignOut ->
            ( model, Effect.fromCmd <| Shared.signOut () )

        GotMaps (Success maps) ->
            ( { model
                | pagingParam = Api.continuePagingParam model.pagingParam maps.nextToken
                , maps = Success maps.items
              }
            , Effect.none
            )

        GotMaps _ ->
            ( model, Effect.none )

        OnLoadAddition ->
            case model.pagingParam.nextToken of
                Nothing ->
                    ( model, Effect.none )

                Just _ ->
                    ( model
                    , Effect.fromCmd <|
                        Api.doPublicApi model.appConfig (\param -> Api.listMaps param model.pagingParam AppendMaps)
                    )

        ColorPickerMsg colorMsg ->
            let
                ( m, colour ) =
                    ColorPicker.update colorMsg (Views.c2c model.editTarget.themeColor) model.colorPicker

                editTarget =
                    model.editTarget
            in
            ( { model
                | colorPicker = m
                , editTarget = { editTarget | themeColor = Maybe.withDefault editTarget.themeColor <| Maybe.map Views.c2c_ colour }
              }
            , Effect.none
            )

        AppendMaps (Success newMaps) ->
            case model.maps of
                Success currentMaps ->
                    ( { model
                        | pagingParam = Api.continuePagingParam model.pagingParam newMaps.nextToken
                        , maps = Success (currentMaps ++ newMaps.items)
                      }
                    , Effect.none
                    )

                _ ->
                    ( { model
                        | pagingParam = Api.continuePagingParam model.pagingParam newMaps.nextToken
                        , maps = Success newMaps.items
                      }
                    , Effect.none
                    )

        AppendMaps _ ->
            ( model, Effect.none )

        ClickDelete ->
            ( model
            , Effect.fromCmd <|
                Api.doProtectedApi model.appConfig
                    (\param ->
                        Api.deleteMap param
                            model.editTarget.id
                            Saved
                    )
            )

        ConfirmDelete map ->
            ( { model | deleteDialogVisible = True, editTarget = map }, Effect.none )

        ClickSave ->
            case model.editorVisible of
                ShowForNew ->
                    ( model
                    , Effect.fromCmd <|
                        Api.doPublicApi model.appConfig
                            (\param ->
                                Api.existsMap param model.editTarget.id CheckIdDuplicate
                            )
                    )

                ShowForUpdate ->
                    ( model
                    , Effect.fromCmd <|
                        Api.doProtectedApi model.appConfig
                            (\param ->
                                Api.updateMap
                                    param
                                    model.editTarget
                                    Saved
                            )
                    )

                Hide ->
                    ( model, Effect.none )

        HideEditor ->
            ( { model
                | editorVisible = Hide
                , deleteDialogVisible = False
                , inputGoogleMapUrl = ""
                , errorMessage = ""
              }
            , Effect.none
            )

        CheckIdDuplicate mMap ->
            case ( model.editorVisible, mMap ) of
                ( Hide, _ ) ->
                    ( model, Effect.none )

                ( ShowForUpdate, _ ) ->
                    ( model, Effect.none )

                ( ShowForNew, Success Nothing ) ->
                    ( model
                    , Effect.fromCmd <|
                        Api.doProtectedApi model.appConfig
                            (\param ->
                                Api.createMap
                                    param
                                    model.editTarget
                                    Saved
                            )
                    )

                ( ShowForNew, Success _ ) ->
                    ( { model | errorMessage = "IDが重複してます。" }, Effect.none )

                ( ShowForNew, Failure err ) ->
                    ( { model | errorMessage = Api.errorString err }, Effect.none )

                ( ShowForNew, _ ) ->
                    ( model, Effect.none )

        ShowEditorForNew ->
            ( { model
                | editorVisible = ShowForNew
                , editTarget = Api.defaultMap
                , inputGoogleMapUrl = ""
                , errorMessage = ""
              }
            , Effect.none
            )

        ShowEditorForUpdate map ->
            ( { model
                | editTarget = map
                , editorVisible = ShowForUpdate
                , inputGoogleMapUrl = ""
                , errorMessage = ""
              }
            , Effect.none
            )

        ShowDetailPage map ->
            ( model, Effect.none )

        Saved _ ->
            ( { model
                | editorVisible = Hide
                , deleteDialogVisible = False
                , errorMessage = ""
              }
            , Effect.fromCmd <| Api.doPublicApi model.appConfig (\param -> Api.listMaps param model.pagingParam GotMaps)
            )

        ChangeInput ope v ->
            ( ope v model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


title : String
title =
    "地図の管理"


view : Model -> View Msg
view model =
    { title = title
    , body =
        Views.layout OnSignOut Nothing Gen.Route.Admin__Map <|
            case model.maps of
                Success _ ->
                    [ h1 [] [ text title ], viewMaps model ]

                _ ->
                    [ h1 [] [ text title ] ]
    }


viewMaps : Model -> Html Msg
viewMaps model =
    DataTable.table
        { columns =
            [ { widthClass = B.is2
              , label = "ID"
              , html = \map -> span [] [ text <| Api.fromId map.id ]
              }
            , { widthClass = "auto"
              , label = "マップ名"
              , html =
                    \map ->
                        let
                            origin =
                                Gen.Route.toHref <| Gen.Route.Map__MapId_ { mapId = Api.fromId map.id }
                        in
                        div []
                            [ span [] [ text map.name ]
                            , br [] []
                            , a
                                [ Attributes.target "kayoinoba-map", href origin ]
                                [ text <| model.appConfig.codeBase ++ origin ]
                            ]
              }
            , { widthClass = "auto"
              , label = "マップタイトル"
              , html = \map -> span [] [ text map.title ]
              }
            ]
        , pagingParam = model.pagingParam
        , onLoadAddition = OnLoadAddition
        , fetchCountList = [ 5, 20, 50, 100 ]
        , changeFetchCount = ChangeInput inputFetchCount
        , confirmDelete = ConfirmDelete
        , filterText = model.filterText
        , changeFilterText = ChangeInput inputFilterText
        , showDetailPage = ShowDetailPage
        , showEditorForNew = ShowEditorForNew
        , hideEditor = HideEditor
        , checkInputForNew = checkInputForNew model
        , clickSave = ClickSave
        , showEditorForUpdate = ShowEditorForUpdate
        , editorVisible = model.editorVisible
        , editor = \_ -> viewEditor model
        , deleteDialogVisible = model.deleteDialogVisible
        , deleteDialog = \_ -> [ p [] [ text "本当に削除していいですか？" ] ]
        , deleteCancel = HideEditor
        , deleteOk = ClickDelete
        }
        (filterMaps model.maps model.filterText)


viewEditor : Model -> List (Html Msg)
viewEditor model =
    case model of
        { editTarget } ->
            [ p [ css [ color (rgb 255 0 0) ] ] [ text model.errorMessage ]
            , Views.oneColumn
                (Views.input
                    (\arg ->
                        { arg
                            | label =
                                case model.editorVisible of
                                    ShowForUpdate ->
                                        "ID(更新時は編集不可)"

                                    _ ->
                                        "ID"
                            , value = Api.fromId model.editTarget.id
                            , placeholder = "アルファベットと-_のみ（URLの一部となります）"
                            , attributes =
                                case model.editorVisible of
                                    ShowForUpdate ->
                                        [ Attributes.disabled True ]

                                    _ ->
                                        []
                        }
                    )
                    (ChangeInput inputMapId)
                )
            , Views.twoColumns
                (Views.input (\arg -> { arg | label = "マップ名", value = editTarget.name }) (ChangeInput inputMapName))
                (Views.input (\arg -> { arg | label = "マップタイトル", value = editTarget.title }) (ChangeInput inputMapTitle))
            , Views.oneColumn
                (Views.input (\arg -> { arg | label = "トップ画面お知らせ", value = editTarget.topInformation }) (ChangeInput inputMapTopInformation))
            , Views.oneColumn <| div [] <| viewColorPicker model
            , hr [] []
            , Views.oneColumn <| div [] <| viewMapZoom model
            , Views.oneColumn <|
                div [] <|
                    Views.location
                        { lat = editTarget.centerLocation.latitude
                        , lon = editTarget.centerLocation.longitude
                        , googleMapUrl = model.inputGoogleMapUrl
                        , latHandler = ChangeInput inputMapLatitude
                        , lonHandler = ChangeInput inputMapLongitude
                        , googleMapUrlHandler = ChangeInput inputGoogleMapUrl
                        }
            , Views.oneColumn <|
                div []
                    [ div [ class B.columns ]
                        [ div [ class B.column ] [ Views.input (\arg -> { arg | label = "カテゴリ1", value = editTarget.category1Name }) (ChangeInput inputMapCategory1Name) ]
                        , div [ class B.column ] [ Views.input (\arg -> { arg | label = "カテゴリ2", value = editTarget.category2Name }) (ChangeInput inputMapCategory2Name) ]
                        , div [ class B.column ] [ Views.input (\arg -> { arg | label = "カテゴリ3", value = editTarget.category3Name }) (ChangeInput inputMapCategory3Name) ]
                        ]
                    ]
            ]


viewMapZoom : Model -> List (Html Msg)
viewMapZoom model =
    [ label [] [ text "地図の初期拡大率(推奨は15、数値が小さいほど広域になります)" ]
    , input
        [ type_ "range"
        , value <| String.fromInt model.editTarget.zoom
        , Attributes.min "13"
        , Attributes.max "22"
        , step "1"
        , onInput (ChangeInput inputMapZoom)
        , css [ display block, Css.width (pct 100), lineHeight (em 2) ]
        ]
        []
    , p [ css [ textAlign center ] ] [ text <| String.fromInt model.editTarget.zoom ]
    ]


viewColorPicker : Model -> List (Html Msg)
viewColorPicker model =
    let
        tc =
            model.editTarget.themeColor
    in
    [ label [] [ text "ヘッダ背景色" ]
    , div [ css [ marginBottom (px 5) ] ]
        [ Html.Styled.fromUnstyled
            (ColorPicker.view (Views.c2c tc) model.colorPicker
                |> Html.map ColorPickerMsg
            )
        , div [ css [ marginBottom (px 5) ] ]
            [ span [] [ text "R" ]
            , input
                [ css colorElemStyle
                , value <| String.fromInt tc.red
                , onInput (ChangeInput inputMapThemeColorRed)
                ]
                []
            , span [] [ text "G" ]
            , input
                [ css colorElemStyle
                , value <| String.fromInt tc.green
                , onInput (ChangeInput inputMapThemeColorGreen)
                ]
                []
            , span [] [ text "B" ]
            , input
                [ css colorElemStyle
                , value <| String.fromInt tc.blue
                , onInput (ChangeInput inputMapThemeColorBlue)
                ]
                []
            ]
        , Views.mapHeader model.editTarget.title model.editTarget.topInformation (Views.c2c tc)
        ]
    ]


checkInputForNew : Model -> Bool
checkInputForNew model =
    (String.trim <| Api.fromId model.editTarget.id)
        /= ""
        && String.trim model.editTarget.name
        /= ""
        && String.trim model.editTarget.title
        /= ""


filterMaps : RemoteResource (List Map) -> String -> List Map
filterMaps mMaps filterText =
    case mMaps of
        Success maps ->
            if String.trim filterText == "" then
                maps

            else
                List.filter
                    (\map ->
                        String.contains filterText (Api.fromId map.id)
                            || String.contains filterText map.name
                            || String.contains filterText map.title
                            || String.contains filterText map.topInformation
                    )
                    maps

        _ ->
            []


colorElemStyle : List Style
colorElemStyle =
    [ Css.width (em 3)
    , marginLeft (px 2)
    , marginRight (px 5)
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


inputGoogleMapUrl : String -> Model -> Model
inputGoogleMapUrl s m =
    case Views.extractLatLonFromGoogleMapUrl s of
        Just ( lat, lon ) ->
            let
                target =
                    m.editTarget
            in
            { m
                | editTarget = { target | centerLocation = { latitude = lat, longitude = lon } }
                , inputGoogleMapUrl = s
            }

        _ ->
            m


inputMapCategory1Name : String -> Model -> Model
inputMapCategory1Name s m =
    let
        target =
            m.editTarget
    in
    { m | editTarget = { target | category1Name = s } }


inputMapCategory2Name : String -> Model -> Model
inputMapCategory2Name s m =
    let
        target =
            m.editTarget
    in
    { m | editTarget = { target | category2Name = s } }


inputMapCategory3Name : String -> Model -> Model
inputMapCategory3Name s m =
    let
        target =
            m.editTarget
    in
    { m | editTarget = { target | category3Name = s } }


inputMapZoom : String -> Model -> Model
inputMapZoom v m =
    case String.toInt v of
        Nothing ->
            m

        Just i ->
            let
                t =
                    m.editTarget
            in
            { m | editTarget = { t | zoom = i } }


inputMapId : String -> Model -> Model
inputMapId v m =
    let
        t =
            m.editTarget
    in
    { m | editTarget = { t | id = Id v } }


inputMapTitle : String -> Model -> Model
inputMapTitle v m =
    let
        t =
            m.editTarget
    in
    { m | editTarget = { t | title = v } }


inputMapName : String -> Model -> Model
inputMapName v m =
    let
        t =
            m.editTarget
    in
    { m | editTarget = { t | name = v } }


inputMapTopInformation : String -> Model -> Model
inputMapTopInformation v m =
    let
        t =
            m.editTarget
    in
    { m | editTarget = { t | topInformation = v } }


inputMapLatitude : String -> Model -> Model
inputMapLatitude s m =
    case String.toFloat s of
        Nothing ->
            m

        Just f ->
            let
                target =
                    m.editTarget

                location =
                    target.centerLocation
            in
            { m | editTarget = { target | centerLocation = { location | latitude = f } } }


inputMapLongitude : String -> Model -> Model
inputMapLongitude s m =
    case String.toFloat s of
        Nothing ->
            m

        Just f ->
            let
                target =
                    m.editTarget

                location =
                    target.centerLocation
            in
            { m | editTarget = { target | centerLocation = { location | longitude = f } } }


inputFilterText : String -> Model -> Model
inputFilterText v m =
    { m | filterText = v }


inputMapThemeColorRed : String -> Model -> Model
inputMapThemeColorRed v m =
    case String.toInt v of
        Nothing ->
            m

        Just i ->
            let
                editTarget =
                    m.editTarget

                tc =
                    editTarget.themeColor
            in
            { m | editTarget = { editTarget | themeColor = { tc | red = i } } }


inputMapThemeColorGreen : String -> Model -> Model
inputMapThemeColorGreen v m =
    case String.toInt v of
        Nothing ->
            m

        Just i ->
            let
                editTarget =
                    m.editTarget

                tc =
                    editTarget.themeColor
            in
            { m | editTarget = { editTarget | themeColor = { tc | green = i } } }


inputMapThemeColorBlue : String -> Model -> Model
inputMapThemeColorBlue v m =
    case String.toInt v of
        Nothing ->
            m

        Just i ->
            let
                editTarget =
                    m.editTarget

                tc =
                    editTarget.themeColor
            in
            { m | editTarget = { editTarget | themeColor = { tc | blue = i } } }
