{-# LANGUAGE TupleSections #-}

module GiantSquid (splitOn) where

import Data.List (find, transpose)
import Data.Maybe (isNothing)

numbers =
  [ 42,
    44,
    71,
    26,
    70,
    92,
    77,
    45,
    6,
    18,
    79,
    54,
    31,
    34,
    64,
    32,
    16,
    55,
    81,
    11,
    90,
    10,
    21,
    87,
    0,
    84,
    8,
    23,
    1,
    12,
    60,
    20,
    57,
    68,
    61,
    82,
    49,
    59,
    22,
    2,
    63,
    33,
    50,
    39,
    28,
    30,
    88,
    41,
    69,
    72,
    98,
    73,
    7,
    65,
    53,
    35,
    96,
    67,
    36,
    4,
    51,
    75,
    24,
    86,
    97,
    85,
    66,
    29,
    74,
    40,
    93,
    58,
    9,
    62,
    95,
    91,
    80,
    99,
    14,
    19,
    43,
    37,
    27,
    56,
    94,
    25,
    83,
    48,
    17,
    38,
    78,
    15,
    52,
    76,
    5,
    13,
    46,
    89,
    47,
    3
  ]

main = do
  str <- readFile "input-day-4.txt"
  let matrices = splitOn null $ map (map read . words) (lines str) :: [[[Int]]]
  let toBingoBoards = map initBoard matrices
  print $ getLastBoardToWin numbers toBingoBoards

grid :: [[Int]]
grid =
  [ [48, 69, 68, 49, 13],
    [25, 14, 30, 74, 89],
    [16, 38, 19, 24, 29],
    [56, 97, 50, 65, 79],
    [57, 52, 05, 27, 76]
  ]

-- Part B
getLastBoardToWin :: [Int] -> [[[(Int, Bool)]]] -> Int
getLastBoardToWin (x : xs) boards =
  if null bs
    then runBingo xs [b]
    else getLastBoardToWin xs unwonBoards
  where
    newBoards = map (`markBoard` x) boards
    unwonBoards@(b : bs) = filter (not . isBoardBingo) newBoards

-- Part A
runBingo :: [Int] -> [[[(Int, Bool)]]] -> Int
runBingo [] _ = 0
runBingo (x : xs) allBoards =
  if isNothing maybeBoard
    then runBingo xs newBoards
    else let (Just b) = maybeBoard in getScore x b
  where
    newBoards = map (`markBoard` x) allBoards
    maybeBoard = find isBoardBingo newBoards

getScore :: Int -> [[(Int, Bool)]] -> Int
getScore num board = num * allFalsesSum
  where
    allFalsesSum = sum $ map (sum . map fst . filter (not . snd)) board

markBoard :: [[(Int, Bool)]] -> Int -> [[(Int, Bool)]]
markBoard [] _ = []
markBoard (row : rest) number = map (markCell number) row : markBoard rest number
  where
    markCell num elem@(a, _)
      | num == a = (a, True)
      | otherwise = elem

isBoardBingo :: [[(Int, Bool)]] -> Bool
isBoardBingo board =
  any areNeighborsCompleted board
    || any areNeighborsCompleted (transpose board)
  where
    areNeighborsCompleted = all snd

initBoard :: [[Int]] -> [[(Int, Bool)]]
initBoard = map (map (,False))

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f list = first : (splitOn f . drop 1) rest
  where
    (first, rest) = break f list
