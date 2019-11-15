{- 
martijn vogelaar & gianni monteban
1047391 & 1047546
-}
module Chain
where

import Satellite
import Data.List
-- Costs and dimensions.

type Cost  =  Integer
type Dim   =  (Integer, Integer)

(<**>) :: Dim -> Dim -> With Cost Dim
(i, j) <**> (j', k)
  | j == j'    =  (i * j * k) :- (i, k)
  | otherwise  =  error "<**>: dimensions do not match"

(<***>) :: With Cost Dim -> With Cost Dim -> With Cost Dim
(c1 :- d1) <***> (c2 :- d2)
  =  (c1 + c + c2) :- d where c :- d =  d1 <**> d2

-- Minimal costs.

minCost :: [Dim] -> With Cost Dim
minCost [a]  =  0 :- a
minCost as   =  minimum [ minCost bs <***> minCost cs | (bs, cs) <- split as ]

split :: [a] -> [([a], [a])]
split []        =  error "split: empty list"
split [_a]      =  []
split (a : as)  =  ([a], as) : [ (a : bs, cs) | (bs, cs) <- split as]

-- minCost [(10, 30), (30, 5), (5, 60)]
-- minCost [ (i, i + 1) | i <- [1 .. 3] ]
-- minCost [ (i, i + 1) | i <- [1 .. 9] ]
func :: [Dim]
func = [(1,2),(2,3),(3,4)]

subMinimumCost :: (size -> size -> With Cost size) -> [size] -> With Cost size
subMinimumCost f (x1 : x2 : []) = (f (x1) (x2))
subMinimumCost f (x1 : x2 : xs) = subMinimumCost f (satellite (f (x1) (x2)) : xs)  

minimumCost   :: (size -> size -> With Cost size) -> [size] -> With Cost size
minimumCost f xs = minimum (map (\x -> subMinimumCost (f) (x)) (permutations xs))
-- optimalChain  :: (size -> size -> With Cost size) -> [size] -> With Cost (With size (Tree size))
