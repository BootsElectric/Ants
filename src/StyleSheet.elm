module StyleSheet exposing (..)

import Style
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Color as Colors

--Representation of style identifiers
type MyStyles
  = StyleOne
  | Button
  | GameOver
  | MainBackground

roboto : Style.Font
roboto =
    Font.importUrl { url = "https://fonts.googleapis.com/css?family=Roboto", name = "roboto" }

stylesheet : Style.StyleSheet MyStyles variation
stylesheet =
  Style.styleSheet
    [ Style.style StyleOne
      [ Color.text Colors.black
      , Color.background Colors.white
      , Font.size 16
      , Font.typeface
        [ roboto
        , Font.font "Ariel"
        , Font.sansSerif
        ]
      ]
    , Style.style Button
      [ Color.text Colors.black
      , Color.background Colors.white
      , Border.all 1
      , Color.border Colors.black
      , Font.size 20
      , Font.typeface
        [ roboto
        , Font.font "Ariel"
        , Font.sansSerif
        ]
      ]
    , Style.style GameOver
      [ Color.text Colors.black
      , Color.background Colors.white
      , Font.size 60
      , Font.bold
      , Font.center
      , Font.typeface
        [ roboto
        , Font.font "Ariel"
        , Font.sansSerif
        ]
      ]
    , Style.style MainBackground
      [ Border.right 2 ]
  ]
