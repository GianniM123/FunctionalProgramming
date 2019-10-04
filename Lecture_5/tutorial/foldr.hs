module Tutorial
where

import Data.List

allTrue :: (a -> Bool) -> [a] -> Bool
allTrue f l = foldr (\x r -> f x && r) True l 

takeIfTrue :: (a-> Bool) -> [a] -> [a]
takeIfTrue f l = foldr (\x r -> if f x then x : r else r) [] l  

factorial :: Int -> Int
factorial i = foldr (*) 1 [2..i]

sine :: Double -> Int -> Double
sine x lim = sum (take lim (listComp x))

sine2 :: Double -> Int -> Double
sine2 x lim = sum (take lim (unFoldrComp x))


listComp :: Double -> [Double]
listComp x = [(-1)^n * x^(2*n+1) / (fromIntegral(factorial (2*n +1))) |n <-[0..] ]

unFoldrComp :: Double -> [Double]
unFoldrComp x = unfoldr (\(sign, xterm, factterm, n) -> Just(sign * xterm / factterm, (sign*(-1), xterm*x*x,factterm * (n+1) * (n+2), n+2))) (1,x,1,1)
