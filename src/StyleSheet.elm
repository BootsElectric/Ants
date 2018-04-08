module StyleSheet exposing (..)

import Style
import Style.Color as Color
import Style.Font as Font
import Color as Colors

--Representation of style identifiers
type MyStyles
  = StyleOne
  | Button

roboto : Style.Font
roboto =
    Font.importUrl { url = "https://fonts.googleapis.com/css?family=Roboto", name = "roboto" }

stylesheet : Style.StyleSheet MyStyles variation
stylesheet =
  Style.styleSheet
    [ Style.style StyleOne
      [ Color.text Colors.black
      , Color.background Colors.white
      , Font.size 20
      , Font.typeface
        [ roboto
        , Font.font "Ariel"
        , Font.sansSerif
        ]
      ]
    , Style.style Button
      [ Color.text Colors.darkGreen
      , Color.background Colors.darkCharcoal
      , Font.size 20
      , Font.typeface
        [ roboto
        , Font.font "Ariel"
        , Font.sansSerif
        ]
      ]
    ]
