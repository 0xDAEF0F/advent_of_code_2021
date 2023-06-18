import Data.Char (digitToInt)

main = do
  content <- readFile "input-day-3.txt"
  print $ solution $ lines content

solution :: [String] -> Int
solution lines = ogr * csr
  where
    ogr = binaryToDecimal $ oxygenGeneratorRating lines
    csr = binaryToDecimal $ co2ScrubberRating lines

-- Todo: Reimplement this functions
fun :: [String] -> Int -> [String]
fun [] _ = []
fun bList idx = filter (\b -> b !! idx == majorityBit) bList
  where
    majorityBit = if numOfSetBits >= length bList - numOfSetBits then '1' else '0'
    numOfSetBits = sum $ map (\b -> digitToInt $ b !! idx) bList

fun' :: [String] -> Int -> [String]
fun' [] _ = []
fun' binaries idx = filter (\b -> b !! idx == minorityBit) binaries
  where
    minorityBit = if numOfSetBits >= length binaries - numOfSetBits then '0' else '1'
    numOfSetBits = sum $ map (\b -> digitToInt $ b !! idx) binaries

oxygenGeneratorRating :: [String] -> String
oxygenGeneratorRating bList = go bList 0
  where
    go [x] _ = x
    go a i = go (fun a i) (i + 1)

co2ScrubberRating :: [String] -> String
co2ScrubberRating bList = go bList 0
  where
    go [x] _ = x
    go a i = go (fun' a i) (i + 1)

binaryToDecimal :: String -> Int
binaryToDecimal = foldl (\acc b -> acc * 2 + if b == '1' then 1 else 0) 0
