{-# LANGUAGE FlexibleInstances #-}
{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

fmap f xs = pure f <*> xs
-}

import System.Random


mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA _ [] = pure []
mapA g (x:xs) = pure (:) <*> g x <*> mapA g xs

divide :: Int -> Int -> Maybe Int
divide _ 0 = Nothing
divide x y = Just (x `div` y)

{-
(:) :: b -> [b] -> [b]
pure :: a -> f a
pure (:) :: f (b -> ([b] -> [b]))
g x :: f b

pure (:) <*> g x :: f ([b] -> [b])

--- for f = Maybe

mapA g [] = Just []
mapA g (x:xs) =
  case (g x) of
    Nothing -> Nothing
    Just b -> case (mapA g xs) of
      Nothing -> Nothing
      Just c -> b : c

Nothing <*> _ = Nothing
_ <*> Nothing = Nothing
Just a <*> Just b = Just (a b)

ma <*> mb = case ma of
  Nothing -> Nothing
  Just a -> case mb of
    Nothing -> Nothing
    Just b -> Just (a b)
-}

---------------------------------------------------------------------------------------------

class Pair f where
  pair :: f a -> f b -> f (a,b)

instance Pair Maybe where
  -- pair :: Maybe a -> Maybe b -> Maybe (a,b)
  pair Nothing _ = Nothing
  pair _ Nothing = Nothing
  pair (Just x) (Just y) = Just (x,y)

instance Pair [] where
  pair xs ys = [(x,y) | x<-xs, y<-ys]

instance Pair IO where
  pair mx my = do
    x <- mx
    y <- my
    return (x,y)

applyFromPair :: (Functor f, Pair f) =>  f (a -> b) -> f a -> f b
applyFromPair fs xs = fmap (\(f,x) -> f x) fxs
  where
  fxs = pair fs xs





class Single f where
  single :: f ()

instance Single Maybe where
  -- single :: Maybe ()
  single = Just ()

instance Single [] where
  single = [()]

instance Single IO where
  single = return ()

instance Single (Either a) where
  single = Right ()

-- fmap :: (a -> b) -> f a -> f b
pureFromSingle :: (Functor f, Single f) => a -> f a
pureFromSingle x = fmap (\() -> x) single

-------------------------------------------------------------------

data Counter a = C Int a
  deriving (Show)

tick :: a -> Counter a
tick x = C 1 x

instance Functor Counter where
  fmap f (C n x) = C n (f x)

instance Applicative Counter where
  pure x = C 0 x
  C m f <*> C n x = C (m+n) (f x)


example = mapA tick [1,2,3]

{-
sortA :: (a -> a -> f Ordering) -> [a] -> f [a]
-}

---------------------------------------------------------------------

class Nondet f where
  choice :: f a -> f a -> f a

instance Nondet [] where
  choice xs ys = xs ++ ys

instance Nondet IO where
  choice xs ys = do
    pick <- randomIO :: IO Bool
    if pick then xs else ys

---------------------------------------------------------------------

{-

class Applicative (f :: * -> *)
Either :: * -> * -> *
Either a :: * -> *


data Either a b = Left a | Right b

instance Applicative (Either a) where
  pure x = Right x
  Left err <*> _ = Left err
  _ <*> Left err = Left err
  Right f <*> Right x = Right (f x)

-}

class Applicative f => Error f where
  bad :: String -> f a

instance Error Maybe where
  bad _msg = Nothing

instance Error (Either String) where
  bad msg = Left msg

instance Error [] where
  bad _msg = []

genericLast :: Error f => [a] -> f a
genericLast [] = bad "empty list"
genericLast [x] = pure x
genericLast (_:xs) = genericLast xs

---------------------------------------------------------------------
{-
Lit 1 :+: Lit 3
4

(Lit 4 :?: Lit 5) :+: Lit 9
[13,14]

evalN :: (Functor f, Nondet f) => f Integer

example1 = (Lit 5 :?: Var "a") :+: Lit 3
evalEN example1 [("a",100)]
 = [8,103]

example2 = (Lit 5 :?: Var "banana") :+: Lit 3
evalEN example2 [("a",100)]
 = [8,3]

-}

----------------------------------------------------------------------

-- pure asdf <*> stuff
-- asdf <$> stuff

filterA :: Applicative f => (a -> f Bool) -> [a] -> f [a]
filterA _ [] = pure []
filterA p (x:xs) = pure (\keepX ys -> if keepX then x:ys else ys) <*> px <*> rest
  where
  px = p x  -- :: f Bool
  rest = filterA p xs -- :: f [a]







