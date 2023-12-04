import qualified Data.Char as C
import qualified Data.List as L
import Data.Maybe (fromJust, isJust, isNothing)
  
numTokens :: [(String, String)]
numTokens = [
  ("zero",  "0"),
  ("one",   "1"),
  ("two",   "2"),
  ("three", "3"),
  ("four",  "4"),
  ("five",  "5"),
  ("six",   "6"),
  ("seven", "7"),
  ("eight", "8"),
  ("nine",  "9")]

strToInt :: String -> Int
strToInt = read
  
tupCat :: (String, String) -> String
tupCat tup = L.intercalate "" [fst tup, snd tup]

maxMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe x Nothing = x
maxMaybe (Just x) (Just y) = Just $ max x y

minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe Nothing y = y
minMaybe x Nothing = x
minMaybe (Just x) (Just y) = Just $ min x y

maxByMaybe :: Ord b => (a -> Maybe b) -> a -> a -> a
maxByMaybe fn x y
  | (isNothing fx) && (isNothing fy) = y
  | otherwise = if maxMaybe fx fy == fx then x else y
  where 
    fx = fn x
    fy = fn y
    
minByMaybe :: Ord b => (a -> Maybe b) -> a -> a -> a
minByMaybe fn x y
  | (isNothing fx) && (isNothing fy) = y
  | otherwise = if minMaybe fx fy == fx then x else y
  where 
    fx = fn x
    fy = fn y

firstDigit :: [(String, (Maybe Int, Maybe Int))] -> String
firstDigit xs =
  fst $ foldl (minByMaybe (\(_,(lo,_)) -> lo)) (head xs) xs

lastDigit :: [(String, (Maybe Int, Maybe Int))] -> String
lastDigit xs =
  fst $ foldl (maxByMaybe (\(_,(_,hi)) -> hi)) (head xs) xs
  
getFirstAndLastDigits :: [(String, (Maybe Int, Maybe Int))] -> (String, String)
getFirstAndLastDigits ts = (firstDigit ts, lastDigit ts)
  
anyFirstLastOccursIndex :: String -> [String] -> (String, (Maybe Int, Maybe Int))
anyFirstLastOccursIndex haystack needles = (
    last needles, (
      anyFirstOccursIndex haystack needles, 
      anyLastOccursIndex haystack needles))

anyFirstOccursIndex :: String -> [String] -> Maybe Int
anyFirstOccursIndex haystack needles = 
  foldl minMaybe Nothing $ map (firstOccurrenceIndex haystack) needles
    
anyLastOccursIndex :: String -> [String] -> Maybe Int
anyLastOccursIndex haystack needles =
  foldl maxMaybe Nothing $ map (lastOccurrenceIndex haystack) needles

firstOccurrenceIndex :: String -> String -> Maybe Int
firstOccurrenceIndex [] _ = Nothing
firstOccurrenceIndex haystack needle
  | L.isPrefixOf needle haystack = Just 0
  | otherwise = case firstOccurrenceIndex (drop 1 haystack) needle of
      Just x -> Just (x+1)
      Nothing -> Nothing
    
lastOccurrenceIndex :: String -> String -> Maybe Int
lastOccurrenceIndex [] _ = Nothing
lastOccurrenceIndex haystack needle
  | L.isSuffixOf needle haystack = Just ((length haystack) - (length needle))
  | otherwise = case lastOccurrenceIndex (init haystack) needle of
      Just x -> Just x
      Nothing -> Nothing
    
allOccurrenceIndex :: String -> String -> [Int]
allOccurrenceIndex [] _ = []
allOccurrenceIndex haystack needle =
  case firstOccurrenceIndex haystack needle of
    Nothing -> []
    Just 0 -> [0] ++ allOccurrenceIndex (drop 1 haystack) needle
    Just x -> [x-1] ++ allOccurrenceIndex (drop x haystack) needle

doIt :: String -> String
doIt input =
  show 
    $ sum 
    $ map (\line -> strToInt $ tupCat $ getFirstAndLastDigits [anyFirstLastOccursIndex line [fst num, snd num] | num <- numTokens]) 
    $ lines input

main :: IO()
main = interact doIt