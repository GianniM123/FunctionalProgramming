module Foldl
where

allTrue :: [Bool] -> Bool
allTrue l = foldr (&&) True l

allFalse :: [Bool] -> Bool
allFalse l = not( foldr (||) True l)

member :: (Eq a) => a -> [a] -> Bool
member x = foldr (\y b -> y == x || b) False

smallest :: [Int] -> Int
smallest = foldr (\x y xs -> if x<y then x else xs) 0
