module Mastermind
where
import System.Random

data Colors = White | Silver | Green | Red | Orange | Pink | Yellow | Blue deriving (Eq,Show)
listColors = [White,Silver,Green,Red,Orange,Pink,Yellow,Blue]




randomColor :: IO Colors
randomColor = randomRIO(0, length listColors-1) >>= return . (listColors !!)


-- randomColors :: Int -> IO[Colors]
-- randomColors 0 = []
-- randomColors x = [randomColor] ++ (randomColors (x-1))

-- createCode :: Int -> 