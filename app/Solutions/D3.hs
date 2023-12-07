module Solutions.D3 (solved3p1, solved3p2) where

import Data.Char
import Control.Lens
import Control.Monad.ST
import Control.Monad
import Data.STRef
import Data.List

findNextDot :: Int -> Int -> String -> Int
findNextDot i w g | i `mod` w == 0 = i
                  | (g !! i) == '.' = i
                  | otherwise = findNextDot (i+1) w g

adjacencyList :: Int -> Int -> [Int]
adjacencyList i w | i `mod` w == 0 = [i-w, i-w+1, i+1, i+w, i+w+1]
                  | i `mod` w == w-1 = [i-w-1, i-w, i-1, i+w-1, i+w]
                  | otherwise = [i-w-1, i-w, i-w+1, i-1, i+1, i+w-1, i+w, i+w+1]

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


interpose :: a -> [a] -> [a]
interpose f s | length s == 8 = concat [take 3 s, [f], take 1 . drop 3 $ s, [f], take 1 . drop 4 $ s, [f], drop 5 s]
                      | otherwise = concat [take 2 s, [f], take 1 . drop 2 $ s, [f], drop 3 s]

hasTwoNumbers :: String -> Bool
hasTwoNumbers [] = False
hasTwoNumbers (c:cs) | isDigit c = (k /= []) && ((==[]) . filter isDigit . dropWhile isDigit $ k)
                     | otherwise = hasTwoNumbers cs
        where k = dropWhile (not . isDigit) . dropWhile isDigit $ cs

inxFirstDigit :: String -> Int -> Maybe Int -> Int
inxFirstDigit _ _ Nothing = (-1)
inxFirstDigit g w (Just i) | i < 0 = 0 -- read . takeWhile isDigit $ g
                           | (i `mod` w == 0) && (isDigit (g!!i)) = i
                           | isDigit (g !! i) = inxFirstDigit g w $ Just (i-1)
                           | otherwise = i+1  -- read . takeWhile isDigit . drop (i+1) $ g

getNumber :: String -> Int -> Int -> Int
getNumber g w i = let startNextRow = ((i `div` w)+1)*w
                   in read . takeWhile isDigit . take (startNextRow - i) . drop i $ g

getNumbersInSurroundings :: Int -> Int -> String -> [Int]
getNumbersInSurroundings i w g = map (getNumber g w) . filter (>=0) . nub . map (inxFirstDigit g w) . filter (isDigit . inxSafe g) . interpose (Nothing) . map Just $ adjacencyList i w
    where
        inxSafe _ Nothing = '3'
        inxSafe m (Just n) = (m !! n)

prodGears :: Int -> String -> [[Int]]
prodGears w g = runST $ do
                    i <- newSTRef 0
                    s <- newSTRef []
                    forM_ g $ \c -> do
                        i' <- readSTRef i
                        s' <- readSTRef s
                        let k = if c == '*' then hasTwoNumbers . interpose '.' . map (g!!) $ adjacencyList i' w else False
                        writeSTRef s $ if k then (getNumbersInSurroundings i' w $ g) : s' else s'
                        writeSTRef i (i'+1)
                    readSTRef s

--foldl1 (+) . map (foldl1 (*)) . 

solved3p2 :: String -> String
solved3p2 x = show . foldl1 (+) . map (foldl1 (*)) . prodGears width . concat $ ls
    where
        ls = lines x
        width = length . head $ ls