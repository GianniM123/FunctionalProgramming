module Main
where
import System.Random
import Control.Monad
import System.IO
import Data.List
import Data.List.Split

data Colors = White | Silver | Green | Red | Orange | Pink | Yellow | Blue | Cyan | Purple | Gold | Black | Grey | Brown  deriving (Eq,Ord,Show)
listColors = [White,Silver,Green,Red,Orange,Pink,Yellow,Blue,Cyan,Purple,Gold,Black,Grey, Brown]

data Symbol = A | B | D | F | G deriving (Eq,Show)
symbolList = [A,B,D,F,G]


main :: IO ()
main = do
    putStrLn "Welcome to Mastermind, how many guesses do you want?"
    maxTries <- readLn
    putStrLn "Okay and how long should the code length be?"
    codeLength <- readLn
    putStr "I think of a "
    putStr . show $ codeLength
    putStr " colors, make a guess you have a maximum of "
    putStr . show $ maxTries
    putStrLn " tries."
    colors <- randomColors codeLength listColors
    y <- gameLoop colors 0 maxTries codeLength
    putStrLn y
    putStr "The solution was: "
    print colors

gameLoop :: [Colors] -> Int -> Int -> Int -> IO String
gameLoop colors tries maxTries codeLength = do
    guess <- getLine
    let guessedColors = map parseInput (splitOn " " guess)
    let correct = checkCorrectInput colors guessedColors 0
    let incorrectValues = removeFromList colors correct 0
    let incorrectGuesses = removeFromList guessedColors correct 0
    let rightColors = (checkCorrectColor incorrectValues incorrectGuesses)
    putStr "Amount of places correct: "
    print (length correct)

    putStr "Amount of colors correct: "
    print rightColors
    putStrLn ""
    if (length correct == codeLength) then
        return "You have won!"
    else
        if (tries >= maxTries-1) then
            return "you have lost!"
        else
            gameLoop colors (tries+1) maxTries codeLength

removeFromList :: [Colors] -> [Int] -> Int -> [Colors]
removeFromList [] [] _ = []
removeFromList x [] _ = x
removeFromList (x:xs) (y:ys) i = if (y == i) then removeFromList xs ys (i+1) else [x] ++ removeFromList xs (y:ys) (i+1)

checkCorrectInput :: [Colors] -> [Colors] -> Int -> [Int]
checkCorrectInput (x:[]) (y:[]) i = if x == y then [i] else []
checkCorrectInput (x:xs) (y:[]) i = error("Unequal length")
checkCorrectInput (x:[]) (y:ys) i = error("Unequal length")
checkCorrectInput (x:xs) (y:ys) i = if x == y then [i] ++ (checkCorrectInput xs ys (i+1)) else (checkCorrectInput xs ys (i+1))

checkInList :: [Colors] -> [[Colors]] -> Int
checkInList [] _ = 0
checkInList _ [] = 0
checkInList x (y:ys) 
 | (head x == head y) = min (length x) (length y)
 | otherwise = checkInList x ys

checkCorrectColor :: [Colors] -> [Colors] -> Int
checkCorrectColor x y = sum (map (\z -> checkInList z nY ) nX)
    where nX = group . sort $ x 
          nY = group . sort $ y

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
 | x == "Cyan" = Cyan
 | x == "Purple" = Purple
 | x == "Gold" = Gold
 | x == "Black" = Black
 | x == "Grey" = Grey
 | x == "Brown" = Brown
 | otherwise = error ("Unknown color")

randomValue :: [Colors] -> IO Colors
randomValue list = randomRIO(0, length list-1) >>= return . (list !!)


randomColors :: Int -> [Colors]-> IO [Colors]
randomColors 0 list = return []
randomColors x list = replicateM x $ randomValue list

-- createCode :: Int -> 