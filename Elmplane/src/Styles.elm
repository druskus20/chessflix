module Styles exposing (..)

import Css exposing (..)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)


boardStyle : Attribute msg
boardStyle =
    css
        [ Css.displayFlex
        ]


rowStyle : Attribute msg
rowStyle =
    css
        [ width (px 40)
        ]


fieldStyle : Attribute msg
fieldStyle =
    css
        [ height (px 40)
        , textAlign center
        , verticalAlign middle
        , fontSize (px 30)
        ]
