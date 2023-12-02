module Main where

import Control.Monad
import Data.Char
import Data.List
import System.IO
import Text.Read (readMaybe)

getFL :: String -> String
getFL line = [head line] ++ [last line]

getDigits :: String -> String
getDigits line = filter isDigit line

strToInt :: String -> Maybe Int
strToInt str = readMaybe str

norm :: Maybe Int -> Int
norm Nothing = 0
norm (Just n) = n

sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs

main :: IO()
main = do
    contents <- getContents
    let calibrationVal = (map (norm . strToInt . getFL . getDigits) (lines contents))
    putStr $ show (sumList calibrationVal)
    