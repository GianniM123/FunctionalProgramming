module Fold
where

allTrue :: [Bool] -> Bool
allTrue l = foldl (&&) (True) (l)

allFalse :: [Bool] -> Bool
allFalse l = not (foldl (||) (False) (l))

member :: (Eq a) => a -> [a] -> Bool
member e = foldr (\x r -> x == e || r) False

smallest :: [Int] -> Int
smallest (x:xs) = foldr (min) x xs

largest :: [Int] -> Int
largest (x:xs) = foldr (max) x xs