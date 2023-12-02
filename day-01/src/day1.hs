module Main where

import Data.Char (isDigit)
import System.IO
import Text.Read (readMaybe)

getFL :: String -> String
getFL line = [head line] ++ [last line]

getDigits :: String -> String
getDigits line = filter isDigit line

strToInt :: String -> Maybe Int
strToInt str = readMaybe str

fromMaybeNum :: Num a => Maybe a -> a
fromMaybeNum Nothing = 0
fromMaybeNum (Just n) = n

sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs

sumCalibrationValues :: String -> String
sumCalibrationValues input = result
    where
        extractValue = fromMaybeNum . strToInt . getFL . getDigits
        result = show $ sumList (map extractValue (lines input))

main :: IO()
main = interact sumCalibrationValues