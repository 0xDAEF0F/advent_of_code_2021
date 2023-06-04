main :: IO ()
main = do
  contents <- readFile "input-day-2.txt"
  print $ go $ lines contents

type State = (Int, Int) -- (x, y)

type State' = (Int, Int, Int) -- (aim, x, y)

go :: [String] -> State'
go = foldl updateState' (0, 0, 0)

updateState :: State -> String -> State
updateState (x, y) str
  | direction == "forward" = (x + pAmt, y)
  | direction == "down" = (x, y + pAmt)
  | direction == "up" = (x, y - pAmt)
  where
    pAmt = read amt
    [direction, amt] = words str

updateState' :: State' -> String -> State'
updateState' (aim, x, y) str
  | direction == "down" = (aim + pAmt, x, y)
  | direction == "up" = (aim - pAmt, x, y)
  | direction == "forward" = (aim, x + pAmt, y + (aim * pAmt))
  where
    pAmt = read amt
    [direction, amt] = words str
