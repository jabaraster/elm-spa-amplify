module Tags exposing (..)

import Bulma.Classes as B
import Html.Styled as H exposing (Attribute, Html)
import Html.Styled.Attributes exposing (..)
import Styles


button : List (Attribute msg) -> List (Html msg) -> Html msg
button attrs =
    H.styled H.button [] (type_ B.button :: attrs)


label : List (Attribute msg) -> List (Html msg) -> Html msg
label =
    H.styled H.label Styles.label


h1 : List (Attribute msg) -> List (Html msg) -> Html msg
h1 =
    H.styled H.h1 Styles.h1
