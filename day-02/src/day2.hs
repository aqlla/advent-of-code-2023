import qualified Data.List as L
import qualified Data.Text as T

strToInt :: String -> Int
strToInt = read

split :: String -> String -> [String]
split delim str = map T.unpack $ T.splitOn (T.pack delim) (T.pack str)

maxi :: Ord a => [a] -> a
maxi [x] = x
maxi (x:x':xs) = maxi ((if x >= x' then x else x'):xs)

getMaxRGB :: Ord a => [[a]] -> [a]
getMaxRGB lists = map maxi . L.transpose $ lists

countColor :: [String] -> String -> Int
countColor [] _ = 0
countColor colorStrs select =
  case filter (\x -> last x == select) (map (split " ") colorStrs) of
    [] -> 0
    [x] -> strToInt $ head x

getGameId :: String -> Int
getGameId lineId = let (_:idStr) = split " " lineId in
  strToInt . last $ idStr

parseLine :: String -> (Int, [[Int]])
parseLine line = 
  let countRGB cubesStr = map (countColor (split ", " cubesStr)) ["red", "green", "blue"]; 
      (idStr:hand:_) = split ": " line in
  (getGameId idStr, map countRGB $ split "; " hand)

isPossible :: [Int] -> Bool
isPossible (r:g:b:_) = 12 >= r && 13 >= g && 14 >= b

filterPossible = filter (\x -> isPossible (getMaxRGB (snd x)))
part1 input = sum . map fst . filterPossible . map parseLine . lines $ input
part2 input = sum . map (\x -> foldr (*) 1 (getMaxRGB . snd $ x)) . map parseLine $ input

main = interact (show . part2 . lines)