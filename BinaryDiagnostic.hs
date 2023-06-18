import Data.HashMap.Internal (HashMap, (!))
import Data.HashMap.Internal qualified as HM

main = do
  contents <- readFile "input-day-3.txt"
  let toLines = lines contents
  print $ solution toLines

example =
  [ "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  ]

bar = foldl foo HM.empty example

solution :: [String] -> String
solution lines = recombineBinary map
  where
    map = foldl foo HM.empty lines

foo :: HashMap Int (Int, Int) -> String -> HashMap Int (Int, Int)
foo hm str = go hm $ zip str [0 ..]
  where
    go hm [] = hm
    go hm ((b, idx) : t) = case HM.lookup idx hm of
      Nothing -> go (HM.insert idx t2 hm) t
      Just tup -> go (HM.insert idx (addTwoTuples tup t2) hm) t
      where
        t2 = if b == '0' then (1, 0) else (0, 1)

addTwoTuples (a, b) (c, d) = (a + c, b + d)

recombineBinary :: HashMap Int (Int, Int) -> String
recombineBinary hm = go $ HM.toList hm
  where
    go [] = []
    go ((_, (a, b)) : xs) = currSym : go xs
      where
        currSym = if a > b then '0' else '1'

invert' :: String -> String
invert' = map foo
  where
    foo '1' = '0'
    foo '0' = '1'
