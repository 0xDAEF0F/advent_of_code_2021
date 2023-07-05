module Lanternfish () where

import Data.Map (Map, findWithDefault, (!))
import Data.Map qualified as M
import InputDaySix (fish)

type Fish = Map Integer Integer

fishes :: Fish
fishes =
  M.fromList
    [(0, 0), (1, 162), (2, 36), (3, 27), (4, 47), (5, 28), (6, 0), (7, 0), (8, 0)]

fishes' :: Fish
fishes' =
  M.fromList
    [(0, 0), (1, 1), (2, 1), (3, 2), (4, 1), (5, 0), (6, 0), (7, 0), (8, 0)]

fishTotal :: Fish -> Integer
fishTotal fish = foldr sumF 0 (M.toList fish)
  where
    sumF (_, v1) v2 = v1 + v2

goDay :: Fish -> Fish
goDay f = returnFishes
  where
    f' = M.fromList (map (\(k, v) -> (k - 1, v)) $ M.toList f)
    spawns = findWithDefault 0 (-1) f'
    returnFishes = M.insert 6 sixes (M.insert 8 spawns (M.delete (-1) f'))
