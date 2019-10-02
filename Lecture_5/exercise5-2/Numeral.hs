module Numeral
where

type Base   =  Integer
type Digit  =  Integer

msdf, lsdf :: Base -> [Digit] -> Integer 
msdf b l = foldl (\x y -> x*b + y) 0 l
lsdf b l = foldr (\x y -> x + y*b) 0 l