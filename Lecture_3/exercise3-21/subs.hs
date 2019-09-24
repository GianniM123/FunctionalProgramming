-- Gianni Monteban & Martijn Vogelaar
-- s1047546 & s1047391

module Subs
where


subs :: [a] -> [[a]] -- 2^n
subs [] = [[]]
subs xs = [ [x] | x <- xs]