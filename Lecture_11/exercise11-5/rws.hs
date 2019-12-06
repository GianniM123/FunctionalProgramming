{-
Gianni Monteban & Martijn Vogelaar
1047546 & 1047391
-}
import Data.Maybe

newtype RWS r w s a = RWS { fromRWS :: r -> s -> (a, s, w) }

instance Functor (RWS r w s) where
-- fmap :: (a -> b) -> RWS r w s a -> RWS r w s b
--      == (a -> b) -> (r -> s -> (a, s, w)) -> (r -> s -> (b, s, w))
   fmap f (RWS rws) = RWS $ \r -> \s -> let (a,s',w) = rws r s in (f a, s', w)  

instance (Monoid w) => Applicative (RWS r w s) where
-- pure :: a -> RWS r w s a
   pure a = RWS $ \r -> \s -> (a, s , mempty)

-- (<*>) :: RWS r w s (a -> b) -> RWS r w s a -> RWS r w s b
   RWS f <*> RWS a = RWS $ \r -> \s -> let (fx,bx,cx) = f r s in
                                       let (ay,by,cy) = a r s in 
                                              (fx ay, s, mappend cx cy) 
                                        

instance (Monoid w) => Monad (RWS r w s) where
   return = pure
-- (<>>=>) :: RWS r w s a -> (a -> RWS r w s b) -> RWS r w s b
   RWS a >>= f =  RWS $ \r -> \s -> let (x,_,_) = a r s in
                                    fromRWS(f x) r s

ask :: (Monoid w) => RWS r w s r
ask = RWS $ \r -> \s -> (r, s, mempty)

get :: (Monoid w) => RWS r w s s
get = RWS $ \_ -> \s -> (s, s, mempty)

put :: (Monoid w) => s -> RWS r w s ()
put s = RWS $ \_ -> \_ -> ((), s, mempty)

tell :: w -> RWS r w s ()
tell w = RWS $ \_ -> \s -> ((), s, w) 

data Expr
  =  Lit Integer    -- a literal
  |  Expr :+: Expr  -- addition
  |  Expr :*: Expr  -- multiplication
  |  Div Expr Expr  -- integer division
  |  Var String     -- variables

type Env   = [(String,Integer)]
type Log   = [String]
type Count = Int

evalRWS :: Expr -> RWS Env Log Count Integer
evalRWS (Lit i)      =  get >>= \c -> put c >> pure i
evalRWS (e1 :+: e2)  =  pure (+)  <*> evalRWS e1 <*> evalRWS e2
evalRWS (e1 :*: e2)  =  pure (*)  <*> evalRWS e1 <*> evalRWS e2
evalRWS (Div e1 e2)  =  pure div  <*> evalRWS e1 <*> evalRWS e2
evalRWS (Var v)      =  RWS $ \env -> \count -> (fromMaybe 0 (lookup v env), count,[v])

p1 = (fromRWS $ evalRWS (Var "a" :+: Lit 1)) [("a", 4711), ("b", 0815)] 0
p2 = (fromRWS $ evalRWS (Var "a" :+: Var "b")) [("a", 4711), ("b", 0815)] 0
p3 = (fromRWS $ evalRWS (Var "a" :+: Var "c")) [("a", 4711), ("b", 0815)] 0
p4 = (fromRWS $ evalRWS (Div (Var "a") (Var "c"))) [("a", 4711), ("c", 0)] 0
