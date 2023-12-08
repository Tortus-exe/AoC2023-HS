module Solutions.D4 (solved4p1, solved4p2) where


import Control.Monad.State.Lazy
import Control.Monad.Loops
import Data.Char
import Data.List

data Card = Card
            { cId :: Int
            , winningNumbers :: [Int]
            , cardNumbers :: [Int] }
            deriving (Show)

parseCardID :: State String Int
parseCardID = do
                inp <- get
                let num = (read :: String -> Int) . takeWhile isDigit . drop 5 $ inp
                put . dropWhile (==' ') . dropWhile (/= ':') $ inp
                return num

parseNumber :: Char -> State String Int
parseNumber m = do
                inp <- get
                let num = (read :: String -> Int) . takeWhile isDigit $ inp
                put . dropWhile (\x->not $ isDigit x || x==m) . dropWhile isDigit $ inp
                return num

parseWinningNumbers :: State String [Int]
parseWinningNumbers = do
                inp <- get
                put . dropWhile (not . isDigit) $ inp
                whileM (((/='|') . head) <$> get) $ parseNumber '|'

parseCardNumbers :: State String [Int]
parseCardNumbers = do
                inp <- get
                put . dropWhile (not . isDigit) $ inp
                whileM ((not . null) <$> get) $ parseNumber 'l'

parseCard' :: State String Card
parseCard' = do 
                c <- parseCardID
                d <- parseWinningNumbers
                e <- parseCardNumbers
                return Card { cId = c, winningNumbers = d, cardNumbers = e }

parseCard :: String -> Card
parseCard = fst . runState parseCard' 

points :: Int -> Int
points 0 = 0
points x = 2^(x-1)

solved4p1 :: String -> String
solved4p1 = show . foldl1 (+) . map (points . length . liftA2 intersect winningNumbers cardNumbers . parseCard) . lines

process' :: [[Int]] -> [Int] -> [Int]
process' k heldCards = let wonCards = concatMap (k!!) heldCards
                        in if wonCards == heldCards then wonCards else wonCards ++ (process' k wonCards)

process :: [Int] -> [Int]
process k = [0..length k-1] ++ process' (zipWith (\x y -> [y+1..y+x] ) k [0..]) [0..length k-1]

-- points . length . liftA2 intersect winningNumbers cardNumbers . 

solved4p2 :: String -> String
solved4p2 = show . length . process . map (length . liftA2 intersect winningNumbers cardNumbers . parseCard) . lines