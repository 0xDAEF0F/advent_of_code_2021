import Data.HashMap.Internal (HashMap, (!))
import Data.HashMap.Internal qualified as HM

type Point = (Int, Int)
type Segment = (Point, Point)

main :: IO ()
main = do
  file <- readFile "input-day-5.txt"
  let segments = map parseLineToSegment $ lines file
  let finalState = markAllSegments segments
  let res = pointsTraversedMoreThanOnce finalState
  print res

pointsTraversedMoreThanOnce :: HashMap Point Int -> Int
pointsTraversedMoreThanOnce hm = length $ filter (\(k, v) -> v > 1) $ HM.toList hm

parseLineToSegment :: String -> Segment
parseLineToSegment str = ((read x1, read y1), (read x2, read y2))
  where
    (x1, rest) = break (== ',') str
    (y1, rest') = break (== '-') (tail rest)
    (x2, rest'') = break (== ',') (tail rest')
    y2 = tail rest''

markAllSegments :: [Segment] -> HashMap Point Int
markAllSegments = foldl markSegment HM.empty

markSegment :: HashMap Point Int -> Segment -> HashMap Point Int
markSegment hm seg@((x1, y1), (x2, y2)) = foldl markPoint hm (allPointsInSegment seg)

allPointsInSegment :: Segment -> [Point]
allPointsInSegment (p1@(x1, y1), p2@(x2, y2))
  | x1 == x2 = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
  | y1 == y2 = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
  | x2 > x1 =
      zip
        [x1 .. x2]
        (if y2 > y1 then [y1 .. y2] else [y1, y1 - 1 .. y2])
  | x1 > x2 =
      zip
        [x1, x1 - 1 .. x2]
        (if y2 > y1 then [y1 .. y2] else [y1, y1 - 1 .. y2])

markPoint :: HashMap Point Int -> Point -> HashMap Point Int
markPoint m p
  | not $ HM.member p m = HM.insert p 1 m
  | otherwise = let c = m ! p in HM.insert p (c + 1) m
