module Ants exposing (..)

type alias Ants =
  { number : Int
  , food : Int
  }

{-|
  The initial numbers for Ants
-}
initialAnts : Ants
initialAnts =
    { number = 1000
    , food = 1000
    }

{-|
  Increase the amount of food in the given set of Ants by the given amount
    ants - the set of ants to update
    amount - the amount to increase the Ants.food by
-}
increaseFood : Ants -> Int -> Ants
increaseFood ants amount =
    { ants | food = ants.food + amount }

{-|
  Decrease the amount of food in the given set of Ants by the given amount
    ants - the set of ants to update
    amount - the amount to decrease the Ants.food by
-}
decreaseFood : Ants -> Int -> Ants
decreaseFood ants amount =
  if ants.food > amount then
      { ants | food = ants.food - amount }
  else
      { ants
      | food = 0
      , number = ants.number + (ants.food - amount)
      }


{-|
  Increase the number of ants in the given set of Ants by the given amount
    ants - the set of ants to update
    amount - the amount to increase the Ants.number by
-}
increaseNumber : Ants -> Int -> Ants
increaseNumber ants amount =
    { ants | number = ants.number + amount }


{-|
  Decrease the number of ants in the given set of Ants by the given amount
    ants - the set of ants to update
    amount - the amount to decrease the Ants.number by
-}
decreaseNumber : Ants -> Int -> Ants
decreaseNumber ants amount =
  if ants.number > amount then
      { ants | number = ants.number - amount }
  else
      { ants | number = 0 }
