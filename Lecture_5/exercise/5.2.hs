module Numeral
where

type Base   =  Integer
type Digit  =  Integer

msdf, lsdf :: Base -> [Digit] -> Integer
msdf base list = foldl (\x y -> x*base + y) 0 list
lsdf base list = foldr (\x y -> x + y*base) 0 list