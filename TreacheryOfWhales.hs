module TreacheryOfWhales () where

import Data.Function (on)
import Data.List (minimumBy, sort)
import GiantSquid (splitOn)

main = do
  input <- readFile "input-day-7.txt"
  let nums = map read $ splitOn (== ',') input :: [Integer]
  let solutionOne = solutionA nums
  let solutionTwo = solutionB nums
  print (solutionOne, solutionTwo)

solutionA :: [Integer] -> Integer
solutionA crabs =
  let position = median crabs
   in sum $ map (\c -> abs (c - position)) crabs

solutionB :: [Integer] -> Integer
solutionB crabs =
  minimum $
    map
      (`calculateFuel` crabs)
      [(minimum crabs) .. (maximum crabs)]
  where
    calculateFuel position crabs = sum allCrabsFuel
      where
        allCrabsFuel = map (\c -> sum [0 .. abs (c - position)]) crabs

median :: [Integer] -> Integer
median xs =
  if odd $ length xs
    then sorted !! mid
    else
      let (a, b) = (sorted !! mid, sorted !! (mid - 1))
       in round $ fromIntegral (a + b) / 2
  where
    sorted = sort xs
    mid = length xs `div` 2
