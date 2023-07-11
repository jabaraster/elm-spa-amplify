module UI exposing (layout, submitter)

import Bulma.Classes as B
import Gen.Route
import Html exposing (div)
import Html.Styled exposing (Html, a, header, li, text, ul)
import Html.Styled.Attributes exposing (class, href, type_)
import Html.Styled.Events exposing (onClick)
import Loading


submitter : msg -> Bool -> String -> Html msg
submitter handler loading label =
    Html.Styled.button
        [ type_ B.button
        , class B.button
        , onClick handler
        , Html.Styled.Attributes.disabled loading
        ]
        [ if loading then
            Html.Styled.fromUnstyled <| Loading.render Loading.DoubleBounce Loading.defaultConfig Loading.On

          else
            text label
        ]


layout : msg -> List (Html msg) -> List (Html.Html msg)
layout signOutOperation children =
    [ Html.Styled.toUnstyled <|
        header []
            [ ul []
                [ li [] [ a [ href <| Gen.Route.toHref Gen.Route.Home_ ] [ text "Home" ] ]
                , li [] [ a [ href <| Gen.Route.toHref Gen.Route.Admin__Map ] [ text "地図" ] ]
                , li [] [ a [ onClick signOutOperation, href "#" ] [ text "サインアウト" ] ]
                ]
            ]
    , div [] <| List.map Html.Styled.toUnstyled children
    ]
