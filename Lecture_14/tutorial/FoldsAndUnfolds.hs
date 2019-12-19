{-# LANGUAGE TypeFamilies #-}
import Squiggol

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs

{-
-- We can not (easily) do this
tailsFoldr :: [a] -> [[a]]
tailsFoldr = foldr f [[]]
  where
  f x tailsXs = _something : tailsXs
-}

-- tails implemented with para
tailsPara :: [a] -> [[a]]
tailsPara = para f
  where
  f :: LIST a ([a], [[a]]) -> [[a]]
  f NIL = [[]]
  f (CONS x (xs,res)) = (x:xs) : res

-------------------------------------------------------------------

-- The (++) operator

concatFold :: [a] -> [a] -> [a]
concatFold xs ys = fold f xs
  where
  f NIL = ys
  f (CONS x zs) = x : zs

{-
-- not (easily) possible
concatUnfold :: [a] -> [a] -> [a]
concatUnfold xs ys = unfold f xs
  where
  f [] = _ys
  f (x:xs') = CONS x xs'
-}

concatApo :: [a] -> [a] -> [a]
concatApo xs ys = apo' f (next xs)
  where
  f (x,xs') = CONS x (next xs')
  next [] = Left ys
  next (x:xs') = Right (x,xs')

apo' :: (Base f) => (seed -> f (Either (Rec f) seed)) -> Either (Rec f) seed -> Rec f
apo' _ (Left stop) = stop
apo' f (Right seed) = apo f seed

-------------------------------------------------------------------

-- para implemented with fold
myPara :: (Base f) => (f (Rec f,res) -> res) -> Rec f -> res
myPara f = snd . fold myF
  where
  --myF :: f (Rec f, res) -> (Rec f, res)
  myF layer = (inn (fmap fst layer), f layer)

-- fold implemented with para
myFold :: (Base f) => (f a -> a) -> (Rec f -> a)
myFold f = para myF
  where
  myF x = f (fmap snd x)

-- apo implemented with unfold
myApo :: (Base f) => (seed -> f (Either (Rec f) seed)) -> seed -> Rec f
myApo f = unfold myF . Right
  where
  myF (Right seed) = f seed
  myF (Left x) = fmap Left (out x)
   -- out x :: f (Rec f)
   -- fmap Left :: f whatever -> f (Either whatever somethingElse)

-------------------------------------------------------------------

-- There can be multiple Base functors for the same datatype

data LISTY a b = NILLY | CONSY a b

instance Functor (LISTY a) where
  fmap _ NILLY = NILLY
  fmap f (CONSY a b) = CONSY a (f b)
instance Base (LISTY a) where
  type Rec (LISTY a) = [a]
  inn NILLY = []
  inn (CONSY x xs) = x:xs
  out [] = NILLY
  out (x:xs) = CONSY x xs

{-
-- identity function as a fold (compiler doesn't accept this because of ambiguous types)
identity :: (Base f) => Rec f -> Rec f
identity = fold inn
-}


-------------------------------------------------------------------

-- Two folds in parallel
{-

(fold f xs, foldr g xs) = fold (two f g) xs

-}
two :: Functor f => (f a -> a) -> (f b -> b) -> (f (a,b) -> (a,b))
two f g x = (f (fmap fst x), g (fmap snd x))

sumF :: LIST Int Int -> Int
sumF NIL = 0
sumF (CONS a b) = a + b

lengthF :: LIST a Int -> Int
lengthF NIL = 0
lengthF (CONS _ n) = n + 1

sumAndLength :: [Int] -> (Int,Int)
--sumAndLength xs = (fold sumF xs, fold lengthF xs)
sumAndLength xs = fold (two sumF lengthF) xs

