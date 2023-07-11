module DataTable exposing
    ( ColumnMeta
    , EditorVisible(..)
    , Param
    , table
    )

import Api exposing (PagingParam)
import Bulma.Classes as B
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Maybe.Extra as Maybe
import Styles
import Svg.Styled.Attributes exposing (x, z)
import Views exposing (IconKind(..))


type EditorVisible
    = Hide
    | ShowForNew
    | ShowForUpdate


type alias Param row msg =
    { columns : List (ColumnMeta row msg)
    , pagingParam : PagingParam
    , fetchCountList : List Int
    , changeFetchCount : String -> msg
    , showDetailPage : row -> msg
    , showEditorForNew : msg
    , showEditorForUpdate : row -> msg
    , filterText : String
    , onLoadAddition : msg
    , changeFilterText : String -> msg
    , confirmDelete : row -> msg
    , editorVisible : EditorVisible
    , editor :
        ()
        -> List (Html msg) -- Elmは遅延実行がないので無駄な関数実行を防ぐ.
    , hideEditor : msg
    , checkInputForNew : Bool
    , clickSave : msg
    , deleteDialogVisible : Bool
    , deleteDialog : () -> List (Html msg)
    , deleteCancel : msg
    , deleteOk : msg
    }


type alias ColumnMeta row msg =
    { widthClass : String
    , label : String
    , html : row -> Html msg
    }


dialogFooterStyle : List Style
dialogFooterStyle =
    [ backgroundColor (rgb 255 255 255)
    , padding (px 4)
    , borderTop3 (px 1) solid (rgba 100 100 100 0.6)
    , position sticky
    , bottom zero
    , left zero
    , right zero
    ]


table : Param row msg -> List row -> Html msg
table param datas =
    div [] <|
        viewTop param
            :: viewHeader param.columns
            :: List.map (viewRow param) datas
            ++ div [ class B.level ]
                [ div [ class B.levelLeft ] []
                , div [ class B.levelRight ]
                    [ div [ class B.levelItem ]
                        [ Views.select
                            { value = Just param.pagingParam.limit
                            , values = param.fetchCountList
                            , valueToString = String.fromInt
                            , valueToLabel = String.fromInt
                            , handler = param.changeFetchCount
                            , attributes = []
                            }
                        ]
                    , div [ class B.levelItem ]
                        [ button
                            [ type_ "button"
                            , class B.button
                            , onClick param.onLoadAddition
                            , Attributes.disabled <| Maybe.isNothing param.pagingParam.nextToken
                            ]
                            [ Views.icon Download, span [] [ text "更に読み込む" ] ]
                        ]
                    ]
                ]
            :: (case param.editorVisible of
                    Hide ->
                        []

                    _ ->
                        [ Views.backdrop []
                            [ Views.form []
                                [ div [ css [ marginBottom (rem 1) ] ] <| param.editor ()
                                , dialogFooter param
                                ]
                            ]
                        ]
               )
            ++ (if param.deleteDialogVisible then
                    [ Views.dialog
                        { html = param.deleteDialog ()
                        , cancel = param.deleteCancel
                        , ok = param.deleteOk
                        }
                    ]

                else
                    []
               )


dialogFooter : Param row msg -> Html msg
dialogFooter param =
    div [ class B.level, css dialogFooterStyle ]
        [ div [ class B.levelLeft ] [] -- ダミーを入れないとlevel-rightが機能しない. 惜しい.
        , div [ class B.levelRight ]
            [ button
                [ type_ "button"
                , Views.concatClass [ B.button, B.levelItem, B.isLink ]
                , Attributes.disabled <| not <| param.checkInputForNew
                , onClick param.clickSave
                ]
                [ Views.icon Save
                , span [] [ text "保存" ]
                ]
            , button
                [ type_ "button"
                , Views.concatClass [ B.button, B.levelItem ]
                , onClick param.hideEditor
                ]
                [ text "キャンセル" ]
            ]
        ]


viewTop : Param row msg -> Html msg
viewTop { showEditorForNew, changeFilterText, filterText } =
    div [ class B.level, css [ marginTop (px 10) ] ]
        [ div [ class B.levelLeft ]
            [ Views.iconButton Plus
                [ class B.isDark
                , onClick showEditorForNew
                ]
            ]
        , div [ class B.levelRight, css [ Css.width (pct 40) ] ]
            [ div [ css [ Css.width (pct 100) ] ]
                [ Views.inputUnderLine
                    [ onInput changeFilterText
                    , value filterText
                    , css [ display inline, Css.width (pct 90) ]
                    ]
                    []
                , Views.iconButton Search []
                ]
            ]
        ]


viewHeader : List (ColumnMeta row msg) -> Html msg
viewHeader columns =
    div (css Styles.th :: rowAttrs) <|
        List.map
            (\column ->
                div
                    [ class B.column
                    , class column.widthClass
                    , css
                        [ fontSize (rem 0.8)
                        , fontWeight bold
                        , color (rgb 150 150 150)
                        ]
                    ]
                    [ text column.label ]
            )
            columns
            ++ [ div [ class B.column, class B.is2 ] [] ]


rowAttrs : List (Attribute msg)
rowAttrs =
    [ class B.columns
    , css
        [ borderBottom3 (px 1) solid (rgba 30 30 30 0.3)
        , hover [ backgroundColor (rgba 255 0 0 0.1) ]
        ]
    ]


viewRow : Param row msg -> row -> Html msg
viewRow { columns, showEditorForUpdate, confirmDelete, showDetailPage } row =
    div rowAttrs <|
        List.map (\column -> div [ class B.column, class column.widthClass ] [ column.html row ]) columns
            ++ [ div [ class B.column, class B.is2 ]
                    [ Views.iconButton Pen [ onClick <| showEditorForUpdate row, css [ Styles.borderNone ] ]
                    , Views.iconButton Trash [ onClick <| confirmDelete row, class B.isDanger, class B.isInverted, css [ Styles.borderNone ] ]
                    , Views.iconButton ArrowRight [ onClick <| showDetailPage row, css [ Styles.borderNone ] ]
                    ]
               ]
