{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Char as C
import qualified Data.List as L
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Text as T
import Text.Read (readMaybe)
-- import qualified Text.Regex as R
import Debug.Trace
import Text.Printf

main :: IO ()
main = do
  let numWords = [("zero",  "0"),("one",   "1"),("two",   "2"),("three", "3"),("four",  "4"),("five",  "5"),("six",   "6"),("seven", "7"),("eight", "8"),("nine",  "9")]
  let ls = ["eight9fhstbssrplmdlncmmqqnklb39ninejz", "three656", "ppjvndvknbtpfsncplmhhrlh5"]
  let dls = map (norm . readMaybe . getFL . getDigits . (replaceAllIn numWords)) ls
  
  putStrLn $ show (sumList dls)
  let ws = map (\(x,y) -> [T.unpack x, T.unpack y]) numWords
  
  -- let flOccursPartial = map (\(x, y) -> (y, anyFirstLastOccursIndex [x,y])) ws
  -- let flOccurs = map (\x -> x)
  
  let occurencesPerLine = map (anyFirstLastOccursIndex . T.unpack) ls
  -- let justFlOccurs = filter (\(_,(f,l)) -> (isJust f) && (isJust l)) flOccurs
  let firstAndLastDigits = map (\o -> (filter (\(x,y) -> isJust x || isJust y) [o num | num <- ws])) occurencesPerLine
  
  -- let flOccurs = map (\(x, y) -> (y, anyFirstLastOccursIndex "eight9fhstbssrplmdlncmmqqnklb39ninejz" [x,y])) ws
  -- let justFlOccurs = filter (\(_,(f,l)) -> (isJust f) && (isJust l)) flOccurs
  -- let firstAndLastDigits = map (\x -> (firstDigit x, lastDigit x)) [justFlOccurs]

  putStrLn $ show firstAndLastDigits
  
  -- putStr $ show (T.replace "eight" "8" "eight9fhstbssrplmdlncmmqqnklb39ninejz")
  -- putStr $ show $ map (replaceAllIn numWords) ls


getFL :: String -> String
getFL line = [L.head line] ++ [L.last line]

getDigits :: String -> String
getDigits line = filter C.isDigit line

norm :: Maybe Int -> Int
norm Nothing = 0
norm (Just n) = n

sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs

minBy :: Ord b => (a -> b) -> [a] -> a
minBy fn list =
  case list of
    (x:y:xs) -> minBy fn (if fn x > fn y then y:xs else x:xs)
    [x] -> x
    [] -> error "Empty list"
    

maxBy :: Ord b => (a -> b) -> [a] -> a
maxBy fn list =
  case list of
    (x:y:xs) -> maxBy fn (if fn x < fn y then y:xs else x:xs)
    [x] -> x
    [] -> error "Empty list"
    
maxByMaybe :: Ord b => (a -> Maybe b) -> a -> a -> a
maxByMaybe fn x y
  | (isNothing fx) && (isNothing fy) = y
  | otherwise = if (maxMaybe fx fy) == fx then x else y
  where 
    fx = fn x
    fy = fn y
    
minByMaybe :: Ord b => (a -> Maybe b) -> a -> a -> a
minByMaybe fn x y
  | (isNothing fx) && (isNothing fy) = y
  | otherwise = if (minMaybe fx fy) == fx then x else y
  where 
    fx = fn x
    fy = fn y

maxMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe x Nothing = x
maxMaybe (Just x) (Just y) = Just $ max x y

minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe Nothing y = y
minMaybe x Nothing = x
minMaybe (Just x) (Just y) = Just $ min x y

firstDigit :: [(String, (Maybe Int, Maybe Int))] -> String
firstDigit xs =
  fst (foldl (minByMaybe (\(_,(lo,_)) -> lo)) (head xs) xs)

lastDigit :: [(String, (Maybe Int, Maybe Int))] -> String
lastDigit xs =
  fst (foldl (maxByMaybe (\(_,(_,hi)) -> hi)) (head xs) xs)
  
anyFirstLastOccursIndex :: String -> [String] -> (Maybe Int, Maybe Int)
anyFirstLastOccursIndex haystack needles =
  (
    anyFirstOccursIndex haystack needles, 
    anyLastOccursIndex haystack needles
  )

anyFirstOccursIndex :: String -> [String] -> Maybe Int
anyFirstOccursIndex haystack needles = first
  where
    occurs = map (firstOccurrenceIndex haystack) needles
    first = foldl minMaybe Nothing occurs
    
anyLastOccursIndex :: String -> [String] -> Maybe Int
anyLastOccursIndex haystack needles = lasst
  where
    occurs = map (lastOccurrenceIndex haystack) needles
    lasst = foldl maxMaybe Nothing occurs

firstOccurrenceIndex :: String -> String -> Maybe Int
firstOccurrenceIndex [] _ = Nothing
firstOccurrenceIndex haystack needle =
  -- if L.isPrefixOf needle (traceShowId haystack)
  if L.isPrefixOf needle haystack
    then (Just 0)
  else case firstOccurrenceIndex (drop 1 haystack) needle of
    Just x -> Just(x+1)
    Nothing -> Nothing
    
lastOccurrenceIndex :: String -> String -> Maybe Int
lastOccurrenceIndex [] _ = Nothing
lastOccurrenceIndex haystack needle =
  -- if L.isPrefixOf needle (traceShowId haystack)
  if L.isSuffixOf needle haystack
    then (Just ((length haystack) - (length needle)))
  else case lastOccurrenceIndex (init haystack) needle of
    Just x -> Just(x)
    Nothing -> Nothing
    
    
allOccurrenceIndex :: String -> String -> [Int]
allOccurrenceIndex [] _ = []
allOccurrenceIndex haystack needle =
  case (firstOccurrenceIndex haystack needle) of
    Nothing -> []
    Just 0 -> [0] ++ (allOccurrenceIndex (drop 1 haystack) needle)
    Just x -> [x-1] ++ (allOccurrenceIndex (drop (x+1) haystack) needle)

-- numOccurrenceIndex :: String -> (String, String) -> 
  


replaceAllIn :: [(T.Text, T.Text)] -> T.Text -> String
replaceAllIn replacements input =
  T.unpack $ foldl (\acc (k, v) -> T.replace k v acc) input replacements

-- replace :: Eq a => [a] -> [a] -> [a] -> [a]
-- replace old new l = T.intercalate new . T.splitOn old $ l