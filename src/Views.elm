module Views exposing
    ( IconKind(..)
    , InputArg
    , backdrop
    , c2c
    , c2c_
    , categoryIcon32
    , colorToRgb
    , concatClass
    , defaultInputArg
    , defaultTextAreaArg
    , dialog
    , extractLatLonFromGoogleMapUrl
    , form
    , icon
    , iconButton
    , iconButtonText
    , iconS
    , input
    , inputUnderLine
    , layout
    , location
    , mapHeader
    , oneColumn
    , oneColumnNoTBMargin
    , oneColumnNoTopMargin
    , select
    , submitter
    , textArea
    , twoColumns
    )

import Api exposing (MapId)
import Api.Enum.PlaceCategory exposing (PlaceCategory(..))
import Api.InputObject
import Bulma.Classes as B
import Bulma.Helpers
import Color
import Css exposing (..)
import Domain exposing (PlaceIconUrls)
import Gen.Route exposing (Route(..))
import Html
import Html.Styled as H exposing (..)
import Html.Styled.Attributes as A exposing (..)
import Html.Styled.Events exposing (..)
import Loading
import Route


{-| Form utility.
-}
form : List (Attribute msg) -> List (Html msg) -> Html msg
form =
    styled H.form
        [ borderRadius (px 4)
        , backgroundColor (rgb 255 255 255)
        , border3 (px 1) solid (rgba 30 30 30 0.3)
        , position absolute
        , padding4 (px 10) (px 10) (px 10) (px 10)
        , top (px 30)
        , right (px 30)
        , left (px 30)
        , maxWidth (px 800)
        , margin2 zero auto
        , maxHeight (vh 90)
        , overflow scroll
        ]


backdrop : List (Attribute msg) -> List (Html msg) -> Html msg
backdrop =
    styled div
        [ backgroundColor (rgba 0 0 0 0.5)
        , position fixed
        , Css.width (vw 100)
        , Css.height (vh 100)
        , top zero
        , left zero
        , zIndex (int 100)
        ]


type alias InputArg msg =
    { value : String
    , label : String
    , placeholder : String
    , type_ : String
    , attributes : List (Attribute msg)
    }


defaultInputArg : InputArg msg
defaultInputArg =
    { value = ""
    , label = ""
    , placeholder = ""
    , type_ = "text"
    , attributes = []
    }


input : (InputArg msg -> InputArg msg) -> (String -> msg) -> Html msg
input argBuilder handler =
    let
        arg =
            argBuilder defaultInputArg

        attrs_ =
            [ class B.input
            , type_ arg.type_
            , placeholder arg.placeholder
            , value arg.value
            , onInput handler
            ]
                ++ arg.attributes
    in
    div []
        [ label [] [ text arg.label ]
        , H.input attrs_ []
        ]


type alias TextAreaArg msg =
    { value : String
    , label : String
    , placeholder : String
    , lineHeight : Int
    , attributes : List (Attribute msg)
    }


defaultTextAreaArg : TextAreaArg msg
defaultTextAreaArg =
    { value = ""
    , label = ""
    , placeholder = ""
    , lineHeight = 6
    , attributes = []
    }


textArea :
    (TextAreaArg msg -> TextAreaArg msg)
    -> (String -> msg)
    -> Html msg
textArea argBuilder handler =
    let
        arg =
            argBuilder defaultTextAreaArg

        attrs_ =
            [ placeholder arg.placeholder
            , class B.textarea
            , value arg.value
            , onInput handler
            ]
                ++ arg.attributes
    in
    div []
        [ label [] [ text arg.label ]
        , textarea attrs_ []
        ]


inputUnderLine : List (Attribute msg) -> List (Html msg) -> Html msg
inputUnderLine attrs =
    let
        tag =
            styled H.input
                [ maxWidth inherit
                , borderStyle none
                , borderRadius zero
                , borderBottom3 (px 1) solid (rgba 0 0 0 0.5)
                , boxShadow none
                ]
    in
    tag <| class B.input :: attrs


{-| Layout utility.
-}
oneColumn : Html msg -> Html msg
oneColumn tag =
    div [ class B.columns ] [ div [ class B.column ] [ tag ] ]


oneColumnNoTopMargin : Html msg -> Html msg
oneColumnNoTopMargin tag =
    div [ class B.columns, css [ marginTop zero ] ] [ div [ class B.column ] [ tag ] ]


oneColumnNoTBMargin : Html msg -> Html msg
oneColumnNoTBMargin tag =
    div [ class B.columns, css [ marginTop zero, marginBottom zero ] ] [ div [ class B.column ] [ tag ] ]


twoColumns : Html msg -> Html msg -> Html msg
twoColumns tag1 tag2 =
    div [ class B.columns ]
        [ div [ class B.column ] [ tag1 ]
        , div [ class B.column ] [ tag2 ]
        ]


select :
    { value : Maybe a
    , values : List a
    , valueToString : a -> String
    , valueToLabel : a -> String
    , handler : String -> msg
    , attributes : List (Attribute msg)
    }
    -> Html msg
select { value, values, valueToString, valueToLabel, handler, attributes } =
    H.select
        (attributes
            ++ [ onInput handler
               , A.value <| Maybe.withDefault "" <| Maybe.map valueToString value
               , class B.input
               ]
        )
    <|
        List.map
            (\optionValue ->
                let
                    valueS =
                        Maybe.withDefault "" <| Maybe.map valueToString value

                    optionValueS =
                        valueToString optionValue
                in
                H.option
                    [ A.value optionValueS
                    , selected <| valueS == optionValueS
                    ]
                    [ text <| valueToLabel optionValue ]
            )
            values


categoryIcon32 : PlaceIconUrls -> PlaceCategory -> Html msg
categoryIcon32 urls category =
    span
        [ css
            [ case category of
                Category1 ->
                    backgroundImage (url urls.category1)

                Category2 ->
                    backgroundImage (url urls.category2)

                Category3 ->
                    backgroundImage (url urls.category3)
            , display inlineBlock
            , Css.width (px 32)
            , Css.height (px 32)
            , backgroundPosition center
            ]
        ]
        []


type alias DialogArg msg =
    { html : List (Html msg)
    , cancel : msg
    , ok : msg
    }


dialog : DialogArg msg -> Html msg
dialog arg =
    backdrop []
        [ form [] <|
            arg.html
                ++ [ hr [] []
                   , button [ type_ "button", class B.button, onClick arg.cancel ] [ text "キャンセル" ]
                   , button [ type_ "button", class B.button, class B.isDanger, onClick arg.ok ] [ icon Check, span [] [ text "OK" ] ]
                   ]
        ]


concatClass : List String -> Attribute msg
concatClass =
    A.fromUnstyled << Bulma.Helpers.classList


type IconKind
    = Redo
    | Plus
    | Search
    | Pen
    | Trash
    | ArrowRight
    | Check
    | Save
    | Download
    | Eye
    | User
    | Home
    | Globe
    | ExternalLinkAlt
    | Image
    | Bars
    | WindowClose


iconName : IconKind -> String
iconName kind =
    case kind of
        Redo ->
            "redo"

        Plus ->
            "plus"

        Search ->
            "search"

        Pen ->
            "pen"

        Trash ->
            "trash"

        Check ->
            "check"

        ArrowRight ->
            "arrow-right"

        Save ->
            "save"

        Download ->
            "download"

        Eye ->
            "eye"

        User ->
            "user"

        Home ->
            "home"

        Globe ->
            "globe"

        ExternalLinkAlt ->
            "external-link-alt"

        Image ->
            "image"

        Bars ->
            "bars"

        WindowClose ->
            "window-close"


icon : IconKind -> Html msg
icon s =
    iconS (iconName s)


iconS : String -> Html msg
iconS s =
    span [ class B.icon ]
        [ i [ class <| "fas fa-" ++ s ] []
        ]


iconButton : IconKind -> List (Attribute msg) -> Html msg
iconButton kind attrs =
    iconCore (iconName kind) "" attrs


iconButtonText : IconKind -> String -> List (Attribute msg) -> Html msg
iconButtonText kind innerText attrs =
    iconCore (iconName kind) innerText attrs


iconCore : String -> String -> List (Attribute msg) -> Html msg
iconCore name innerText attrs =
    let
        attrs_ =
            css [ borderRadius (pct 50) ]
                -- B.isRoundedよりこちらの方がきれいな丸になる
                :: class B.button
                :: type_ B.button
                :: attrs
    in
    button attrs_
        [ iconS name
        , text innerText
        ]


mapHeader : String -> String -> Color.Color -> Html msg
mapHeader title topInformation bc =
    let
        ( r, g, b ) =
            colorToRgb bc
    in
    header
        [ css
            [ padding2 (px 13) (px 10)
            , textAlign center
            , color (rgb 230 230 230)
            , fontSize (px 20)
            , fontWeight (int 700)
            , backgroundColor (rgb r g b)
            ]
        ]
        [ h2 [ css [ fontWeight bold ] ] [ text title ]
        , h4 [ css [ fontSize (Css.em 0.8) ] ] [ text topInformation ]
        ]


c2c : Api.InputObject.ColorInput -> Color.Color
c2c src =
    Color.fromRgba
        { red = toFloat src.red / 255
        , green = toFloat src.green / 255
        , blue = toFloat src.blue / 255
        , alpha = src.alpha
        }


c2c_ : Color.Color -> Api.InputObject.ColorInput
c2c_ src =
    let
        { alpha } =
            Color.toRgba src

        ( r, g, b ) =
            colorToRgb src
    in
    { red = r, green = g, blue = b, alpha = alpha }


colorToRgb : Color.Color -> ( Int, Int, Int )
colorToRgb c =
    let
        rgba =
            Color.toRgba c
    in
    ( ceiling (rgba.red * 255)
    , ceiling (rgba.green * 255)
    , ceiling (rgba.blue * 255)
    )


location :
    { lat : Float
    , lon : Float
    , googleMapUrl : String
    , latHandler : String -> msg
    , lonHandler : String -> msg
    , googleMapUrlHandler : String -> msg
    }
    -> List (Html msg)
location { lat, lon, googleMapUrl, latHandler, lonHandler, googleMapUrlHandler } =
    [ oneColumn
        (input
            (\arg -> { arg | label = "緯度軽度抽出(Google MapのURLを貼り付け)", value = googleMapUrl })
            googleMapUrlHandler
        )
    , twoColumns
        (input
            (\arg -> { arg | label = "緯度", value = String.fromFloat lat })
            latHandler
        )
        (input
            (\arg -> { arg | label = "経度", value = String.fromFloat lon })
            lonHandler
        )
    ]


extractLatLonFromGoogleMapUrl : String -> Maybe ( Float, Float )
extractLatLonFromGoogleMapUrl url =
    let
        cnv =
            \( latS, lonS ) ->
                case ( String.toFloat latS, String.toFloat lonS ) of
                    ( Just lat, Just lon ) ->
                        Just ( lat, lon )

                    _ ->
                        Nothing
    in
    case String.split "/" url |> List.filter (\s -> String.contains "@" s) of
        [ s ] ->
            case String.split "," <| String.dropLeft 1 s of
                [ latS, longS, _ ] ->
                    cnv ( latS, longS )

                [ latS, longS ] ->
                    cnv ( latS, longS )

                _ ->
                    Nothing

        _ ->
            Nothing


submitter : msg -> Bool -> String -> Html msg
submitter handler loading labelText =
    H.button
        [ type_ B.button
        , class B.button
        , onClick handler
        , A.disabled loading
        ]
        [ if loading then
            H.fromUnstyled <| Loading.render Loading.DoubleBounce Loading.defaultConfig Loading.On

          else
            text labelText
        ]


active : Gen.Route.Route -> Gen.Route.Route -> List (Attribute msg)
active r0 r2 =
    if r0 == r2 then
        [ class B.isActive ]

    else
        []


layout : msg -> Maybe MapId -> Gen.Route.Route -> List (Html msg) -> List (Html.Html msg)
layout signOutOperation mMapId route children =
    [ H.toUnstyled <|
        div []
            [ nav [ class B.tabs ]
                [ ul []
                    [ li (active route Admin__Kayoinoba) [ a [ href <| Route.adminKayoinobaHref mMapId ] [ text <| "通いの場" ] ]
                    , li (active route Admin__Map) [ a [ href <| Route.adminMapHref ] [ text "地図" ] ]
                    , li (active route Admin__Place) [ a [ href <| Route.adminPlaceHref mMapId ] [ text "場所" ] ]
                    , li [] [ a [ onClick signOutOperation, href "#" ] [ text "サインアウト" ] ]
                    ]
                ]
            , div
                [ class B.column
                , class B.isFourFifths
                , css
                    [ maxWidth (px 1024)
                    , marginTop zero
                    , marginBottom zero
                    , marginRight auto
                    , marginLeft auto
                    ]
                ]
                children
            ]
    ]
