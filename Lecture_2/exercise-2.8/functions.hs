module Func
where

import Data.Char
import Data.Bool

(<:=) :: [a]->(Int,a)->[a]
(<:=) x y = x

--f1 :: String -> Char -> Int -> Int
f1 :: Eq a => [a] -> a -> Int -> Int
f1 x y z
 | z < 0 || z >= length x = -1
 | x !! z == y = z
 | otherwise = f1 x y (z+1)

-- f2 :: String -> Char -> Int
f2 :: Eq a => [a] -> a -> Int
f2 x y = f1 x y 0

f3 :: String -> Int -> String
f3 x y
 | y < 0 || y >= length x = x
 | map (isUpper) x !! y = f3 (x <:=(y,toLower (x !! y))) (y+1)
 | map (isLower) x !! y = f3 (x <:=(y,toUpper (x !! y))) (y+1)
 | otherwise = f3 x (y+1)
 
f4 :: String -> String
f4 x = f3 x 0

f5 :: [a] -> Int -> Int -> [a]
f5 x y z
 | y > z = x
 | y < 0 = x
 | z >= length x = x
 | otherwise = f5 (x<:=(z,x !! y) <:= (y,x !! z)) (y+1) (z-1)

f6 :: [a] -> [a]
f6 x = f5 x 0 (length x - 1)
-- main = do