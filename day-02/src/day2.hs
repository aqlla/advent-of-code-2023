import qualified Data.List as L
import qualified Data.Text as T
import Data.Maybe (fromJust, isJust, isNothing)

strToInt :: String -> Int
strToInt = read

split :: String -> String -> [String]
split delim str = map T.unpack $ T.splitOn (T.pack delim) (T.pack str)

extractColor :: [String] -> String -> Int
extractColor [] _ = 0
extractColor colors select =
  case filter (\x -> last x == select) (map (split " ") colors) of
    [] -> 0
    [x] -> strToInt $ head x

extractRGB :: String -> [Int]
extractRGB hand = map (extractColor (split ", " hand)) ["red", "green", "blue"]

parseLine :: String -> (Int, [[Int]])
parseLine line = do
  let (idStr:hands) = split ": " line
  (strToInt $ last $ split " " idStr, 
   map extractRGB $ split "; " (head hands))

main :: IO()
main = interact (\input -> show $ map parseLine $ lines $ input)