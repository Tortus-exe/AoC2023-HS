module Solutions.D2 (solved2p1, solved2p2) where

import Data.Char

data Cube = Cube
        { numRed :: Int
        , numGreen :: Int
        , numBlue :: Int
        } deriving (Show, Eq)

data Game = Game
        { gameId :: Int
        , gameMoves :: [Cube]
        } deriving (Show, Eq)

instance Ord Cube where
    (Cube { numRed = r, numGreen = g, numBlue = b }) <= (Cube { numRed = r', numGreen = g', numBlue = b' }) =
        (r <= r') && (g <= g') && (b <= b')

insertColour :: String -> Int -> Game -> Game
insertColour s n g = case s of
                       "red"   -> g { gameMoves = [firstMove { numRed = n }] <> tail moves}
                       "blue"  -> g { gameMoves = [firstMove { numBlue = n }] <> tail moves}
                       "green" -> g { gameMoves = [firstMove { numGreen = n }] <> tail moves}
                       _ -> g
        where
            moves = gameMoves g
            firstMove = head moves

movesParser :: (String, Game) -> (String, Game)
movesParser (s, g) | s == [] = (s, g)
                   | isDigit . head $ s = movesParser (s', insertColour colour num g)
                   | head s == ';' = movesParser (drop 2 s, g { gameMoves = [Cube { numRed = 0, numGreen = 0, numBlue = 0 }] <> gameMoves g})
                   | head s == ',' = movesParser (drop 2 s, g)
                   | otherwise = (s, g)
        where
            num = (read :: String -> Int) . takeWhile isDigit $ s
            colour = takeWhile (`notElem` ",;") . tail . dropWhile isDigit $ s
            s' = dropWhile (`notElem` ",;") s

idParser :: (String, Game) -> (String, Game)
idParser (s, g) | take 5 s == "Game " = (drop 2 . dropWhile (/= ':') $ s, g { gameId = gid })
                | otherwise = (s, g)
        where
            gid = (read :: String -> Int) . takeWhile isDigit . drop 5 $ s

parseGame :: String -> Game
parseGame s = snd . movesParser . idParser $ (s, Game { gameId = 0, gameMoves = [Cube { numRed = 0, numGreen = 0, numBlue = 0 }] })

gamePossible :: Cube -> Game -> Bool
gamePossible c g = foldl1 (&&) . map (<= c) . gameMoves $ g

restrictions :: Cube
restrictions = Cube { numRed = 12, numGreen = 13, numBlue = 14 }

solved2p1 :: String -> String
solved2p1 = show . foldl1 (+) . map gameId . filter (gamePossible restrictions) . map parseGame . lines

maxEach :: Cube -> Cube -> Cube
maxEach Cube{numRed = r, numGreen = g, numBlue = b} Cube{numRed = r', numGreen = g', numBlue = b'} = Cube{numRed = max r r', numGreen = max g g', numBlue = max b b'}

minPossible :: Game -> Cube
minPossible = foldl1 maxEach . gameMoves

power :: Cube -> Int
power c = numRed c * numGreen c * numBlue c

solved2p2 :: String -> String
solved2p2 = show . foldl1 (+) . map (power . minPossible . parseGame) . lines