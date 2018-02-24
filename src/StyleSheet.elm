module StyleSheet exposing (..)

import Style
import Style.Color as Color
import Style.Font as Font
import Color as Colors

--Representation of style identifiers
type MyStyles
  = StyleOne


stylesheet : Style.StyleSheet MyStyles variation
stylesheet =
  Style.styleSheet
    [ Style.style StyleOne
      [ Color.text Colors.black
      , Color.background Colors.white
      , Font.size 20
      , Font.typeface
        [ Font.importUrl { url = "https://fonts.googleapis.com/css?family=Roboto", name = "roboto" }
        , Font.font "Ariel"
        , Font.sansSerif
        ]
      ]
    ]
