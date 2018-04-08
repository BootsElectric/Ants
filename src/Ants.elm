module Ants exposing (..)

type alias Ants =
  { number : Float
  , birthRate : Float
  , deathRate : Float
  , food : Float
  , eatRate : Float
  }

initialAnts : Ants
initialAnts =
    { number = 1000
    , birthRate = 0
    , deathRate = 0.1
    , food = 10
    , eatRate = 0.05
    }

increase : Ants -> Float -> Ants
increase ants amount =
    { ants | food = ants.food + amount }

decrease : Ants -> Float -> Ants
decrease ants amount =
    { ants | food = ants.food - amount }

noAntsLeft : Ants -> Bool
noAntsLeft ants =
    ants.number <= 0

calculatePopulation : Ants -> Ants
calculatePopulation ants =
    let
        br = ants.birthRate

        dr = ants.deathRate

        f = ants.food

        er = ants.eatRate

        n = ants.number

        p =
          n + br - dr

        newF =
          if f > 0 then
            f - er
          else
            0

        newBr =
          if f > 0 then
              f / n
          else
            0

    in

      { ants
      | number = p
      , food = newF
      , birthRate = newBr
       }
