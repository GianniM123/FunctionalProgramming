{-
Gianni Monteban & Martijn Vogelar
1047546 & 1047391
-}

module Evaluator
where
import Data.Maybe

infixl 6 :+:
infixl 7 :*:
infixr 1 :?:

data Expr
  =  Lit Integer    -- a literal
  |  Expr :+: Expr  -- addition
  |  Expr :*: Expr  -- multiplication
  |  Div Expr Expr  -- integer division
  |  Expr :?: Expr  -- non-deterministic choice
  |  Var String     -- a variable

evalA :: (Applicative f) => Expr -> f Integer
evalA (Lit i)      =  pure i
evalA (e1 :+: e2)  =  pure (+)  <*> evalA e1 <*> evalA e2
evalA (e1 :*: e2)  =  pure (*)  <*> evalA e1 <*> evalA e2
evalA (Div e1 e2)  =  pure div  <*> evalA e1 <*> evalA e2

toss  ::  Expr
toss  =  Lit 0 :?: Lit 1

evalN :: Expr -> [Integer]
evalN (Lit i)      =  pure i
evalN (e1 :+: e2)  =  pure (+)  <*> evalN e1 <*> evalN e2
evalN (e1 :*: e2)  =  pure (*)  <*> evalN e1 <*> evalN e2
evalN (Div e1 e2)  =  pure div  <*> evalN e1 <*> evalN e2
evalN (e1 :?: e2)  =  evalN e1 ++ evalN e2

-- 
-- evalN toss
-- evalN (toss :+: Lit 2 :*: toss)
-- evalN (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: toss)))

newtype Environ a = EN { fromEN :: [(String, Integer)] ->  a }

instance Functor Environ where
-- fmap :: (a -> b) -> Environ a -> Environ b
   fmap f (EN x) = EN $ \env -> f (x env) 

instance Applicative Environ where
-- pure a -> Environ a
   pure x = EN $ \env -> x 
    
-- (<*>) :: Environ (a -> b) -> Environ a -> Environ b 
   EN f <*> EN x  = EN $ \ev -> func ev
    where func = \env -> f env (x env)

instance Monad Environ where
-- return a -> Environ a
   return     = error "return (Monad Environ): not yet implemented"
-- (>>=) :: Environ a -> (a -> Environ a) -> Environ b =
   EN m >>= f = error "<*> (Monad Environ): not yet implemented"

evalR :: Expr -> Environ Integer
evalR (Lit i)      =  pure i
evalR (e1 :+: e2)  =  pure (+)  <*> evalR e1 <*> evalR e2
evalR (e1 :*: e2)  =  pure (*)  <*> evalR e1 <*> evalR e2
evalR (Div e1 e2)  =  pure div  <*> evalR e1 <*> evalR e2
evalR (Var v) = EN (\env -> fromMaybe 0 $ lookup v env)
-- evalR = error "evalR: not yet implemented"
-- evalR (Var "a" :+: Lit 1) [("a", 4711), ("b", 0815)]
-- evalR (Var "a" :*: Var "b") [("a", 4711), ("b", 0815)]
-- evalR (Var "a" :*: Var "c") [("a", 4711), ("b", 0815)]


newtype EnvND a = EnN { fromEnN :: [(String, Integer)] ->  [a] }
instance Functor EnvND where
-- fmap :: (a -> b) -> EnvND a -> EnvND b
   fmap f (EnN x) = EnN $ \en -> map (\y -> f y) (x en) 

instance Applicative EnvND where
--pure a -> EnvND a
  pure x = EnN $ \env -> [x] 
--(<*>) :: EnvND (a -> b) -> EnvND a -> EnvND b 
  EnN f <*> EnN xs  = EnN $ \ev -> func ev
    where func = \env -> [y z | y <- (f env), z <- (xs env)]

evalNR :: Expr -> EnvND Integer
evalNR (Lit i)      =  pure i
evalNR (e1 :+: e2)  =  pure (+)  <*> evalNR e1 <*> evalNR e2
evalNR (e1 :*: e2)  =  pure (*)  <*> evalNR e1 <*> evalNR e2
evalNR (Div e1 e2)  =  pure div  <*> evalNR e1 <*> evalNR e2
evalNR (e1 :?: e2)  =  EnN $ \e -> fromEnN (evalNR e1) e ++ fromEnN (evalNR e2) e
evalNR (Var v) = EnN $ \list -> [fromMaybe 0 $ lookup v list]

