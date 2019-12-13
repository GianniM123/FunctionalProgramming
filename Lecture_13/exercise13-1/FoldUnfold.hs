{-# LANGUAGE LambdaCase  #-}

{- 
Gianni Monteban & Martijn Vogelaar
1047546 & 104391
-}

import Prelude hiding (foldr) 

type LIST a = Maybe (a,[a])

out :: [a] -> LIST a
out []      = Nothing
out (x:xs)  = Just (x,xs)

inn :: LIST a -> [a]
inn Nothing       = []
inn (Just (x,xs)) = x:xs

foldr   :: (a -> b -> b) -> b -> ([a] -> b)
foldr f e = consume where
  consume []     = e
  consume (x:xs) = f x (consume xs)
 
foldRAsfoldr :: (Maybe (a,b) -> b) -> ([a] -> b)
foldRAsfoldr f = foldr func (f Nothing)
   where func a b = f (Just(a,b))

foldrAsfoldR :: (a -> b -> b) -> b -> ([a] -> b)
foldrAsfoldR f e = foldR func 
   where func x = case x of
                  Nothing -> e
                  Just(y,ys) -> f y ys 

foldR   :: (Maybe (a,b) -> b) -> ([a] -> b)
foldR al   = consume where
   consume = al . fmap (fmap consume) . out
   --  ([a] -> b) . Maybe (a,[a]) 

unfoldR :: (b -> Maybe (a, b)) -> (b -> [a])
unfoldR co = produce where
   produce = inn . fmap (fmap produce) . co


data LISTB elem res = NIL | CONS elem res

mbP2LB :: Maybe (elem,res) -> LISTB elem res
mbP2LB Nothing = NIL
mbP2LB (Just (e,r)) = CONS e r

lB2MbP :: LISTB elem res -> Maybe(elem,res)
lB2MbP NIL = Nothing
lB2MbP (CONS e r) = (Just (e,r))

instance Functor (LISTB elem) where
   -- (a -> b) -> LISB elem a -> LISTB elem b
   fmap f (NIL) = NIL 
   fmap f (CONS e r) = CONS e (f r)

outB :: [a] -> LISTB a [a]
outB [] = NIL
outB (x:xs) = CONS x xs

innB :: LISTB a [a] -> [a]
innB NIL = []
innB (CONS x xs) = (x:xs)

-- foldRB :: (LISTB a b -> b) -> [a] -> b
foldRB al   = consume where
   consume = al . fmap consume . outB

-- unfoldRB :: (b -> LISTB a b) -> b -> [a]
unfoldRB co = consume where
   consume = innB . fmap consume . co

emap :: (t -> elem) -> LISTB t res -> LISTB elem res
emap f NIL        = NIL
emap f (CONS a b) = CONS (f a) b


mapAsFold :: (a -> b) -> [a] -> [b]
mapAsFold f a = foldRB (innB . emap f) a
-- mapAsFold f a = innB . emap f . outB a
mapAsunfold :: (a -> b) -> [a] -> [b]
mapAsunfold f a = unfoldRB (\l -> emap f (outB l) ) a