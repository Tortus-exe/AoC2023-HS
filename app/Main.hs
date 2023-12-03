module Main where

import System.Environment
import Solutions.D1

dayTable :: [String -> String]
dayTable = [
            solved1p1, 
            solved1p2
           ]

errorMessage :: String
errorMessage = unlines $ ["invalid input! Please input formatted:", 
                          "./solve [day] [part] [file]",
                          "where [day] is from 1 to 25 [part] is either 1 or 2, and [file] is the input for that part."]

executeDay :: [String] -> String -> String
executeDay (d:p:_) input = let inx = 2 * (read d) + (read p)
                           in (dayTable !! inx) $ input
executeDay _ _ = error "invalid arguments!"

main :: IO ()
main = do
        a <- getArgs
        if length a /= 3 then putStrLn errorMessage
        else do
            input <- readFile $ a !! 2
            putStrLn . executeDay a $ input
