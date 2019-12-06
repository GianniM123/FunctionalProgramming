{-
Gianni Monteban & Martijn Vogelaar
1047546 & 1047391
-}

import Parser
import Control.Applicative
import Data.Char

infixl 9 :@

data Lambda var
  =  Var var                   -- variable
  |  Fun var (Lambda var)      -- abstraction/λ-expression
  |  Lambda var :@ Lambda var  -- application
  deriving (Show)

{-
S -> V | F | L
V -> v 
F -> \\v -> S
L -> SS

Var var = var
Fun var (Lambda var) = \\var -> var
Lambda var :@ Lambda var = var var
-}

readString :: Parser String
readString = do
  x <- letter
  xs <- many letter
  return (x:xs)

parseArrow :: Parser ()
parseArrow = do
  x0 <- space
  x1 <- char '-'
  x2 <- char '>'
  x3 <- space
  return ()

parseToLambda :: String -> Lambda String
parseToLambda (x:xs)
 | isSpace x = parseToLambda xs
 | isAlpha x = let var = ((parse readString (x:xs)) !! 0) in if length (snd var) > 0 then Var (fst var) :@ parseToLambda (snd var) else Var (fst var)
 | x == '\\' = let var = (parse readString xs) !! 0 in let var2 = snd( parse parseArrow(snd var) !! 0) in Fun (fst var) (parseToLambda var2) 
 | otherwise = error ("no clue what is happening")                                           

