module Unfold
where

import Prelude hiding (take, filter)
import qualified Data.List as L
import Data.Either (Either(..))


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

take :: Int -> [a] -> [a]
take i l = unfoldr (\(x:xs) -> if  i < size - length xs then Nothing else Just(x,xs)) l where size = length l

subFilter :: (a -> Bool) -> [a]-> Maybe (a,[a])
subFilter _ [] = Nothing
subFilter f (x:xs) = if f x then Just (x,xs) else subFilter f xs

filter :: (a -> Bool) -> [a] -> [a]
filter f l = unfoldr (subFilter f ) l

subFibs :: [Integer] -> Maybe (Integer, [Integer])
subFibs [] = Just (1, [1])
subFibs (x:xs)  
  | length xs == 0 = Just(1, [1] ++ [x])
  | otherwise = Just(a, b ) where a = x + head xs
                                  b = [head xs, a ]
 

fibs :: [Integer]
fibs = unfoldr (subFibs) []

generateNewPrime :: Integer -> [Integer] -> Maybe(Integer, [Integer])
generateNewPrime i l = if elem (0) (map (\x -> i `mod` x) l) == False then Just(i, (l ++ [i])) else generateNewPrime (i+1) (l)


primes :: [Integer]
primes = 2: unfoldr (\(l) -> generateNewPrime ((last l)+1) (l)) [2]

--apo :: (t -> Either [a] (a, t)) -> t -> [a]
-- unfoldr2 :: (t -> Maybe (a, t)) -> t -> [a]
-- unfoldr2 f t = apo (maybeToEither(f)) (t)

-- the ++ operator
add :: [a] -> [a] -> [a]
add l1 l2 = apo (\l -> if length l == 0 then Left l2 else Right(head l, tail l) ) l1

insert :: (Ord a) => a -> [a] -> [a]
insert i list = apo (\(x:xs) -> if i <= x then Left(i : (x:xs)) else if length xs == 0 then Left(x : (i:xs)) else Right (x, xs) ) list