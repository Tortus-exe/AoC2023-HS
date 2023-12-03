module Solutions.D1 (solved1p1, solved1p2) where

import Data.Char
import Data.List

toTwoDigit :: String -> Int
toTwoDigit "" = 0
toTwoDigit c = (read :: String -> Int) . flip map [head, last] $ ($ c)

solved1p1 :: String -> String
solved1p1 = show . foldr1 (+) . map (toTwoDigit . filter isNumber) . lines

nums :: [String]
nums = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

fromJust :: Maybe Int -> Char
fromJust (Just x) = intToDigit (x+1)
fromJust Nothing = 'a'

wordsToNumbers :: String -> String
wordsToNumbers [] = []
wordsToNumbers s@(c:cs) | isNumber c = c : wordsToNumbers cs
                        | word3 `elem` nums = (fromJust $ elemIndex word3 nums) : (wordsToNumbers cs) 
                        | word4 `elem` nums = (fromJust $ elemIndex word4 nums) : (wordsToNumbers cs) 
                        | word5 `elem` nums = (fromJust $ elemIndex word5 nums) : (wordsToNumbers cs) 
                        | otherwise = c : wordsToNumbers cs
    where
        word3 = take 3 s
        word4 = take 4 s
        word5 = take 5 s

findNumberWordAndRead :: String -> Int
findNumberWordAndRead "" = 0
findNumberWordAndRead s = toTwoDigit . filter isNumber . wordsToNumbers $ s

solved1p2 :: String -> String
solved1p2 = show . foldr1 (+) . map findNumberWordAndRead . lines