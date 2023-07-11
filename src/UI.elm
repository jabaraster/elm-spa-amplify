module UI exposing (layout)

import Gen.Route
import Html exposing (Html, div)
import Html.Styled exposing (a, header, li, text, ul)
import Html.Styled.Attributes exposing (href)
import Html.Styled.Events exposing (onClick)


layout : msg -> List (Html.Styled.Html msg) -> List (Html msg)
layout signOutOperation children =
    [ Html.Styled.toUnstyled <|
        header []
            [ ul []
                [ li [] [ a [ href <| Gen.Route.toHref Gen.Route.Home_ ] [ text "Home" ] ]
                , li [] [ a [ href <| Gen.Route.toHref Gen.Route.Admin__Map ] [ text "地図" ] ]
                , li [] [ a [ onClick signOutOperation ] [ text "サインアウト" ] ]
                ]
            ]
    , div [] <| List.map Html.Styled.toUnstyled children
    ]
