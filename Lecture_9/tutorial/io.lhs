> module Main
> where
> import System.Random
> import System.IO
> import Control.Monad

f1 = putStrLn x
      where x = getLine

> f2 = do
>	x <- getLine
>	putStrLn x

f3 = putStrLn (getLine ++ getLine)

f4 = do
	x <- getLine
	y <- getLine
	z < x ++ y
	putStrLn z

> f5 = do
>	x <- getLine
>	y <- getLine
>	let z = x ++ y
>	putStrLn z


f6 = length . getLine

return :: a -> IO a


> pickANumber :: IO Int
> pickANumber = randomRIO (1,100)

putStrLn :: String -> IO ()
getLine :: IOString
readln :: Read a => IO a


> feedback :: Int -> Int -> (String, Bool)
> feedback secret guess
>	| guess < secret = ("Too low", False)
>	| guess > secret = ("Too high", False)
>	| otherwise = ("correct",True)

> gameLoop :: Int -> IO()
> gameLoop secret = do
>	putStr "guess > "
>	guess <- readLn
>	let (msg, done) = feedback secret guess
>	print msg
>	unless done $ gameLoop secret 

> main :: IO()
> main = do
>	hSetBuffering stdout NoBuffering
>	secret <- pickANumber
>	putStrLn "I am thinking of a random number between 1 and 100. Try to guess it"
>	gameLoop secret


sequene :: [IO a] -> [a]

> mapMs :: (a -> IO b) -> [a] -> IO [b]

mapM f l = sequence (map f l)

> mapMs f [] = return []
> mapMs f (x : xs) = do
>	h <- f x
>	hs <- mapMs f xs
>	return (h : hs)

> filterMs :: (a -> IO Bool) -> [a] -> IO [a]
> filterMs p [] = return []
> filterMs p (x :xs) = do
>	keep <- p x
>	kept <-filterMs p xs
>	if keep then 
>		return (x : kept)
>	else
>		return kept
