{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs  #-}

module Tutorial where

import           Control.Monad (join)
import           Prelude       hiding (ap)

-- First, is this readable?
-- Monads! (Don't be scared.)

-- Do notation is just syntactic sugar over (>>=)!

listDo :: [Int]
listDo = do
  x <- [2, 3, 5]
  y <- [7, 11, 13, 10]
  pure (x * y)

-- is equivalent to:

listBind :: [Int]
listBind =
  [2, 3, 5] >>= (\x ->
    [7, 11, 13] >>= (\y ->
      pure (x * y)))

-- Is it clear to everyone how the (implicit) brackets are placed here?

--------------------------------------------------------------------------------

-- An example: zip tree:

data Tree a = Node (Tree a) a (Tree a) | Empty deriving (Show)

zipTree :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree _ (Node _ _ _) Empty = Nothing
zipTree _ Empty (Node _ _ _) = Nothing
zipTree _ Empty Empty        = Just Empty
zipTree f (Node l1 x r1) (Node l2 y r2) =
  case zipTree f l1 l2 of
    Nothing -> Nothing
    Just l  -> case zipTree f r1 r2 of
                  Nothing -> Nothing
                  Just r  -> Just $ Node l (f x y) r

-- Can we write this using the Monad?

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing f  = Nothing
bindMaybe (Just x) f = f x

bindWithPure :: Maybe a -> (a -> b) -> Maybe b
bindWithPure = flip fmap

-- bindMaybe === (>>=)

zipTree' :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree' _ (Node _ _ _) Empty            = Nothing
zipTree' _ Empty (Node _ _ _)            = Nothing
zipTree' _ Empty Empty                   = Just Empty
zipTree' f (Node l1 x r1) (Node l2 y r2) = do
  l' <- zipTree' f l1 l2
  r' <- zipTree' f r1 r2
  pure $ Node l' (f x y) r'
-- zipTree' f (Node l1 x r1) (Node l2 y r2) =
--     zipTree' f l1 l2 >>= (\l' -> zipTree' f r1 r2 >>= (\r' -> Just $ Node l' (f x y) r'))

-- Are all instances of Monad also an instance of Applicative? Yes! Why?

-- ap as it is defined in the Prelude:

ap m1 m2 = do
  x1 <- m1
  x2 <- m2
  return (x1 x2)

-- Recall that this is just sugar over (>>=).

-- instance Monad m => Applicative m where
--   pure = return
--   (<*>) = ap

-- Are all Applicative Monad?

--------------------------------------------------------------------------------

-- What should (<*>) and pure do?
-- Expected behaviour:

-- >>> ZL [(+1), (+2)] <*> [0, 3, 10]
-- ZL [1, 5]

newtype ZipList a =ZL  { runZipList :: [a ]} deriving (Show, Eq, Functor)

-- Just because an instance typechecks doesn't mean it's also correct!

-- instance Functor ZipList where
--   fmap f (ZL [])     = ZL []
--   fmap f (ZL (x:xs)) = ZL (f x : f x : runZipList (fmap f (ZL xs)))

-- runZipList :: ZipList a -> [a]

listDo' :: [(Int, Int)]
listDo' = do
  x <- [2, 3, 5]
  y <- [7, 11, 13, 10]
  pure (x, y)

instance Applicative ZipList where
  pure x = ZL $ repeat x
  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  ZL fs <*> ZL xs = ZL [f x | (f, x) <- zip fs xs]

zl = pure (+) <*> ZL [1,2,3] <*> ZL [4,5,6]

-- (<$>) === fmap

-- runZipList :: ZipList a -> [a]

-- Can we make this into a monad? After all, we already have a Monad instance for []!

instance Monad ZipList where
  return = pure
  ZL xs >>= f = ZL (((>>=) :: [a] -> (a -> [b]) -> [b]) xs (runZipList . f))

-- What's wrong here?

-- The Monad laws:

-- return a  >>= f         ≡   f a
-- m         >>= return    ≡   m
-- (m >>= f) >>= g         ≡   m >>= (\x -> f x >>= g)

-- ap should match (<*>)!
-- Does it in this case?

--------------------------------------------------------------------------------

-- This was allegedly promised in the lecture:

collapse :: Monad m => m (m a) -> m a
collapse x = do
  x' <- x
  x'

-- Available in standard library!

collapse' :: Monad m => m (m a) -> m a
collapse' = join

-- collapse [[1], [1]] = [1, 1]

-- Can we write this?

starGT :: Applicative f => f a -> f b -> f b
starGT x y = (\x' y' -> y') <*> x <*> y
