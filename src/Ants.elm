module Ants exposing (..)

type alias Ants =
  { number : Int
  , food : Int
  }

initialAnts : Ants
initialAnts =
    { number = 1000
    , food = 1000
    }

increaseFood : Ants -> Int -> Ants
increaseFood ants amount =
    { ants | food = ants.food + amount }

decreaseFood : Ants -> Int -> Ants
decreaseFood ants amount =
  if ants.food > amount then
      { ants | food = ants.food - amount }
  else
      { ants | food = 0 }
    

increaseNumber : Ants -> Int -> Ants
increaseNumber ants amount =
    { ants | number = ants.number + amount }

decreaseNumber : Ants -> Int -> Ants
decreaseNumber ants amount =
  if ants.number > amount then
      { ants | number = ants.number - amount }
  else
      { ants | number = 0 }

noAntsLeft : Ants -> Bool
noAntsLeft ants =
    ants.number <= 0
