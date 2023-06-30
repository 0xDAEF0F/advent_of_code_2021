import Data.HashMap.Internal (HashMap, (!))
import Data.HashMap.Internal qualified as HM

main :: IO ()
main = do
  file <- readFile "input-day-5.txt"
  let segments = map parseLineToSegment $ lines file
  let verHorSegments = straightSegments segments
  let finalState = markAllSegments verHorSegments
  let res = pointsTraversedMoreThanOne finalState
  print res

type Point = (Int, Int)

type Segment = (Point, Point)

pointsTraversedMoreThanOne :: HashMap Point Int -> Int
pointsTraversedMoreThanOne hm = length $ filter (\(k, v) -> v > 1) $ HM.toList hm

parseLineToSegment :: String -> Segment
parseLineToSegment str = ((read x1, read y1), (read x2, read y2))
  where
    (x1, rest) = break (== ',') str
    (y1, rest') = break (== '-') (tail rest)
    (x2, rest'') = break (== ',') (tail rest')
    y2 = tail rest''

markAllSegments :: [Segment] -> HashMap Point Int
markAllSegments = foldl markStraightSegment HM.empty

markStraightSegment :: HashMap Point Int -> Segment -> HashMap Point Int
markStraightSegment hm ((x1, y1), (x2, y2))
  | isHorizontal = markPoints [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
  | otherwise = markPoints [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
  where
    isHorizontal = y2 == y1
    markPoints = foldl markPoint hm

straightSegments :: [Segment] -> [Segment]
straightSegments = filter verHor
  where
    verHor :: Segment -> Bool
    verHor ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

markPoint :: HashMap Point Int -> Point -> HashMap Point Int
markPoint m p
  | not $ HM.member p m = HM.insert p 1 m
  | otherwise = let c = m ! p in HM.insert p (c + 1) m
