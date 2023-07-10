module UI exposing (layout)

import Html exposing (Html, div)
import Html.Styled


layout : List (Html.Styled.Html msg) -> List (Html msg)
layout children =
    [ div [] <| List.map Html.Styled.toUnstyled children
    ]
