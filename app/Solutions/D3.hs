module Solutions.D3 (solved3p1, solved3p2) where

import Data.Char
import Control.Lens

findNextDot :: Int -> Int -> String -> Int
findNextDot i w g | i `mod` w == 0 = i
                  | (g !! i) == '.' = i
                  | otherwise = findNextDot (i+1) w g

adjacencyList :: Int -> Int -> [Int]
adjacencyList i w | i `mod` w == 0 = [i+1, i-w, i+w, i-w+1, i+w+1]
                  | i `mod` w == w = [i-1, i-w, i+w, i-w-1, i+w-1]
                  | otherwise = [i-1, i+1, i-w, i+w, i-w-1, i-w+1, i+w-1, i+w+1]

findDotsAroundNumber :: Int -> Int -> String -> (Int, Bool)
findDotsAroundNumber i w g | isDigit k = if surroundedByDigitsOrDots then findDotsAroundNumber (i+1) w g else (findNextDot (i+1) w g, False)
                           | k=='.' = (i+1, True)
                           | otherwise = (findNextDot (i+1) w g, False)
    where
        k = g !! i
        surroundedByDigitsOrDots = foldl1 (&&) . map ((`elem` "0123456789.") . (g !!)) . filter (\x -> x>0 && x<(length g)) $ adjacencyList i w

clearNumber :: Int -> Int -> String -> String
clearNumber i w g | i == w = g
                  | isDigit k = clearNumber (i+1) w $ set (element i) '.' g
                  | otherwise = g
    where
        k = g !! i

destroyIfAlone :: Int -> Int -> String -> String
destroyIfAlone i w g | i == length g = g
                     | isDigit k = destroyIfAlone nexti w $ if dotsAllAround then clearNumber i w g else g
                     | otherwise = destroyIfAlone (i+1) w g
    where
        k = g !! i
        (nexti, dotsAllAround) = findDotsAroundNumber i w g

sumStr :: String -> Int
sumStr x | x == [] = 0
         | isDigit . head $ x = (read . takeWhile isDigit $ x) + (sumStr . dropWhile isDigit $ x)
         | otherwise = sumStr . tail $ x

solved3p1 :: String -> String
solved3p1 x = show . sumStr . destroyIfAlone 0 width . concat $ ls
    where 
        ls = lines x
        width = length . head $ ls


solved3p2 :: String -> String
solved3p2 = undefined