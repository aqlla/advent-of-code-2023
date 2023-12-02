module Main where

import Data.Char (isDigit)
import qualified Data.Map
import System.IO
import Text.Read (readMaybe)

getFL :: String -> String
getFL line = [head line] ++ [last line]

getDigits :: String -> String
getDigits line = filter isDigit line

maybeNum :: Num a => Maybe a -> a
maybeNum Nothing = 0
maybeNum (Just n) = n

sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs

sumCalibrationValues :: String -> String
sumCalibrationValues input = show 
    $ sumList 
    $ map (maybeNum . readMaybe . getFL . getDigits) 
    $ lines input

main :: IO()
main = interact sumCalibrationValues