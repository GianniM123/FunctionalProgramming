module Unfold
where

import Prelude hiding (take)
import qualified Data.List as L

unfoldr :: (t -> Maybe (a, t)) -> t -> [a]
unfoldr rep seed = produce seed
  where
    produce seed = case rep seed of 
       Just (a, new_seed) -> a : produce new_seed
       Nothing            -> []

apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo rep seed = produce seed
  where
    produce seed = case rep seed of 
       Left l     -> l
       Right(a,ns) -> a : produce ns




divides :: Int -> Int -> Bool
divides a b = b `rem` a == 0

sieve :: [Int] -> Maybe (Int, [Int])
sieve [] = Nothing
sieve (x:xs) = Just (x, filter (not . divides x) xs)

primes :: Int -> [Int]
primes n = unfoldr sieve [2 .. n]


subTake :: Int -> [a] -> Maybe(a,[a])
subTake _ [] = Nothing
subTake n (x:xs)
 | length xs >= n = Just(x, xs)
 | otherwise = Nothing

take :: Int -> [a] -> [a]
take n list = unfoldr (subTake ((length list)-n)) list

subFilter :: (a-> Bool) -> [a] -> Maybe(a,[a])
subFilter _ [] = Nothing
subFilter f (x:xs) = if f x then Just(x,xs) else subFilter f xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f l = unfoldr (subFilter f) l


subFibs :: [Integer] -> Maybe(Integer,[Integer]) 
subFibs ([]) = Just(1, [1])
subFibs (x:xs)
 | null xs = Just (1,[1,1])
 | otherwise = Just (x + xs !! 0, xs ++ [x + xs !! 0])


fibs :: [Integer]
fibs = unfoldr subFibs []

abc :: Int -> [Integer]
abc a = take a fibs