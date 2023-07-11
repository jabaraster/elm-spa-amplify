module Styles exposing
    ( block
    , borderNone
    , cursorPointer
    , errorMessage
    , formSmall
    , label
    , list
    , objectFitCover
    , selectableBackground
    , th
    )

import Css exposing (..)


formSmall : List Style
formSmall =
    [ padding (em 1), maxWidth (px 600) ]


cursorPointer : Style
cursorPointer =
    cursor pointer


th : List Style
th =
    [ fontSize (rem 0.8)
    , fontWeight bold
    , color (rgb 150 150 150)
    ]


objectFitCover : Style
objectFitCover =
    Css.property "object-fit" "cover"


borderNone : Style
borderNone =
    border zero


label : List Style
label =
    [ fontWeight bolder
    , color <| rgb 120 120 120
    ]


selectableBackground : Bool -> Style
selectableBackground selected =
    if selected then
        backgroundColor (rgba 255 200 200 0.5)

    else
        backgroundColor (rgba 150 150 150 0.5)


errorMessage : List Style
errorMessage =
    [ color (rgb 255 0 0) ]


block : Style
block =
    display Css.block


list : List Style
list =
    [ listStyle none, paddingLeft zero ]
