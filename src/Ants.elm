module Ants exposing (..)

type alias Ants =
  { number : Int }

initialAnts : Ants
initialAnts =
    { number = 1000 }

increase : Ants -> Int -> Ants
increase ants int =
    { number = ants.number + int }

decrease : Ants -> Int -> Ants
decrease ants int =
    { number = ants.number - int }

noAntsLeft : Ants -> Bool
noAntsLeft ants =
    ants.number <= 0
