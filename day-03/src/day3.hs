import Data.Char (isDigit)
import qualified Data.Text as T
import Data.List

data PartNum = PartNum { value    :: Int
                       , position :: (Int, Int)
                       , len      :: Int }

instance Show PartNum where
    show (PartNum v p l) = show v ++ "-" ++ show p ++ "-" ++ show l

split :: String -> String -> [String]
split delim str = map T.unpack $ T.splitOn (T.pack delim) (T.pack str)

substring start len = take len . drop start

containsAny :: [String] -> String -> Bool
containsAny needles haystack = foldl (\x y -> x || (isInfixOf y haystack)) False needles

containsSymbol :: String -> Bool
containsSymbol haystack = foldl (\x y -> x || isSymbol y) False haystack

isSymbol :: Char -> Bool
isSymbol ch = not (isDigit ch) && not (ch == '.')

mapIdx :: (a -> Int -> b) -> [a] -> [b]
mapIdx f xs = zipWith f xs [0..]

parseLines :: [String] -> [[PartNum]]
parseLines lines = mapIdx extractNumbersFromLineInit lines

extractNumbersFromLineInit :: String -> Int -> [PartNum]
extractNumbersFromLineInit line i = extractNumbersFromLine line i 0

extractNumbersFromLine :: String -> Int -> Int -> [PartNum]
extractNumbersFromLine [] _ _ = []
extractNumbersFromLine line i p =
    case length num of
        0 -> []
        x -> [PartNum (read num) (i, position + p) len] ++ extractNumbersFromLine (drop (len + position) line) i nextPos
    where
        preNum = takeWhile (not . isDigit) line
        position = length preNum
        num = takeWhile isDigit (drop (length preNum) line)
        len = length num
        nextPos = len + position + p

getNeighborsSameLine :: String -> String -> [Char]
getNeighborsSameLine line numStr = neighs
    where 
        (x:y:_) = split numStr line
        first = if length x > 0 then [last x] else []
        neighs = first ++ if length y > 0 then [head y] else []

getNeighborsOtherLine :: [String] -> (Int, Int) -> Int -> [Char]
getNeighborsOtherLine grid (posY, posX) len
    | posY >= length grid = []
    | posY < 0 = []
    | otherwise = case (posX, posX + len + 1) of 
        (0, _) -> take (len + 1) line
        (start, end) -> if end > (length line) 
            then drop (start - 1) line
            else substring (start - 1) (len + 2) line
        where 
            line = grid !! posY

getNeighbors :: [String] -> PartNum -> [Char]
getNeighbors grid partNum = neighbors
    where
        numStr = show (value partNum)
        pos = position partNum
        neighbors = (getNeighborsOtherLine grid ((fst pos) - 1, snd pos) (len partNum))
          ++ (getNeighborsSameLine (grid !! (fst pos)) numStr)
          ++ (getNeighborsOtherLine grid ((fst pos) + 1, snd pos) (len partNum))

part1 lines = res
    where 
        pNums = concat . parseLines $ lines
        getNeighGrid = getNeighbors lines
        res = sum $ map fst $ (filter (\(x,n) ->  containsSymbol n) (map (\x -> (value x, getNeighGrid x)) pNums))

main = do
    interact (show . part1 . lines)