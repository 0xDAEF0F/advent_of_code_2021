import Data.List (tails)

main :: IO ()
main = do
  file <- readFile "input-day-1.txt"
  let numbers = read <$> lines file :: [Int]
  let toComp = sonarSweep 3 numbers
  let onlyGT = filter (== GT) toComp
  print $ length onlyGT

sonarSweep :: Int -> [Int] -> [Ordering]
sonarSweep n xs = zipWith foo subLists (tail subLists)
  where
    subLists = filter ((== n) . length) (map (take n) (tails xs))
    foo a b = sum b `compare` sum a
