module Evaluator
where

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
evalN = error "evalN: not yet implemented"
-- evalN toss
-- evalN (toss :+: Lit 2 :*: toss)
-- evalN (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: toss)))

newtype Environ a = EN { fromEN :: [(String, Integer)] ->  a }

instance Functor Environ where
-- fmap :: (a -> b) -> Environ a -> Environ b
   fmap f (EN x) = EN $ \env -> f (x env) 

instance Applicative Environ where
-- pure a -> Environ a
   pure x = error "pure (Applicative Environ): not yet implemented"
-- (<*>) :: Environ (a -> b) -> Environ a -> Environ b 
   EN f <*> EN x  = error "<*> (Applicative Environ): not yet implemented"

instance Monad Environ where
-- return a -> Environ a
   return     = error "return (Monad Environ): not yet implemented"
-- (>>=) :: Environ a -> (a -> Environ a) -> Environ b =
   EN m >>= f = error "<*> (Monad Environ): not yet implemented"

evalR :: Expr -> Environ Integer
evalR = error "evalR: not yet implemented"
-- evalR (Var "a" :+: Lit 1) [("a", 4711), ("b", 0815)]
-- evalR (Var "a" :*: Var "b") [("a", 4711), ("b", 0815)]
-- evalR (Var "a" :*: Var "c") [("a", 4711), ("b", 0815)]


newtype EnvND a = EnN { fromEnN :: [(String, Integer)] ->  [a] }
instance Functor EnvND where
-- fmap :: (a -> b) -> EnvND a -> EnvND b
   fmap f (EnN x) = error "fmap (Functor EnvND): not yet implemented"

instance Applicative EnvND where
--pure a -> EnvND a
  pure x = error "pure (Applicative EnvND): not yet implemented"
--(<*>) :: EnvND (a -> b) -> EnvND a -> EnvND b 
  EnN f <*> EnN xs  = error "<*> (Applicative EnvND): not yet implemented"

evalNR :: Expr -> EnvND Integer
evalNR = error "evalNR: not yet implemented"

