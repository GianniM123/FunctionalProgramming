{-
Gianni Monteban & Martijn Vogelaar
1047546 & 1047391
-}

import Parser
import Control.Applicative

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
Lambda var :@ Lambda var = varvar
-}

parseToLambda :: String -> Lambda String
parseToLambda (x:xs)
 | x == '\\' = undefined