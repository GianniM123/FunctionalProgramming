module Main
where
import System.Random
import Control.Monad
import Data.List.Split

data Colors = White | Silver | Green | Red | Orange | Pink | Yellow | Blue deriving (Eq,Show)
listColors = [White,Silver,Green,Red,Orange,Pink,Yellow,Blue]

data Symbol = A | B | D | F | G deriving (Eq,Show)
symbolList = [A,B,D,F,G]

codeLength = 4
maxTries = 12

main :: IO ()
main = do
    putStr "I think of a "
    putStr . show $ codeLength
    putStr " colors, make a guess you have a maximum of "
    putStr . show $ maxTries
    putStrLn " tries."
    colors <- randomColors codeLength listColors
    -- print colors
    gameLoop colors 0

gameLoop :: [Colors] -> Int -> IO()
gameLoop colors tries = do
    guess <- getLine
    let guessedColors = map parseInput (splitOn " " guess)
    let correct = checkCorrectInput colors guessedColors
    let rightColors = ((checkCorrectColor colors guessedColors) - correct)
    putStr "Amount of places correct: "
    print correct

    putStr "Amount of colors correct: "
    print rightColors
    if(correct == codeLength || tries >= maxTries-1) then
        return ()
    else
       gameLoop colors (tries+1)

checkCorrectInput :: [Colors] -> [Colors] -> Int
checkCorrectInput (x:[]) (y:[]) = if x == y then 1 else 0
checkCorrectInput (x:xs) (y:[]) = error("Unequal length")
checkCorrectInput (x:[]) (y:ys) = error("Unequal length")
checkCorrectInput (x:xs) (y:ys) = if x == y then 1+(checkCorrectInput xs ys) else 0+(checkCorrectInput xs ys)

checkCorrectColor :: [Colors] -> [Colors] -> Int
checkCorrectColor (x:[]) y = length (filter (==x) y)
checkCorrectColor (x:xs) y = length (filter (==x) y) + (checkCorrectColor xs y)
    
parseInput :: String -> Colors
parseInput x
 | x == "White" = White
 | x == "Silver" = Silver
 | x == "Green" = Green
 | x == "Red" = Red
 | x == "Orange" = Orange
 | x == "Pink" = Pink
 | x == "Yellow" = Yellow
 | x == "Blue" = Blue
 | otherwise = error ("Unknown color")

randomValue :: [Colors] -> IO Colors
randomValue list = randomRIO(0, length list-1) >>= return . (list !!)


randomColors :: Int -> [Colors]-> IO [Colors]
randomColors 0 list = return []
randomColors x list = replicateM x $ randomValue list

-- createCode :: Int -> 