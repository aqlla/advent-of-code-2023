{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List as L
import qualified Data.Text as T
import Data.Maybe (fromJust, isJust, isNothing)

strToInt :: String -> Int
strToInt = read

extractColor :: [T.Text] -> T.Text -> Int
extractColor [] _ = 0
extractColor colors select = 
  case filter (\x -> last x == select) colorsSplit of
    [] -> 0
    [x] -> strToInt $ T.unpack $ head x
  where
    colorsSplit = map (T.splitOn " ") colors

extractRGB :: T.Text -> [Int]
extractRGB hand = map (extractColor (T.splitOn ", " hand)) ["red", "green", "blue"]

parseLine :: String -> (Int, [[Int]])
parseLine line = (
    strToInt $ T.unpack $ last $ T.splitOn " " idStr,
    map extractRGB $ T.splitOn "; " (head hands))
  where 
    (idStr:hands) = T.splitOn ": " (T.pack line)

doIt :: String -> String
doIt input = show $ map parseLine $ lines $ input

main :: IO()
main = interact doIt