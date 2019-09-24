-- Gianni Monteban & Martijn Vogelaar
-- s1047546 & s1047391

module QuickTest (Probes, Property, (?->), (?=>))
where
import Data.List (sort)
import Prelude hiding ((<*>))

type Probes a    =  [a]

type Property a  =  a -> Bool

infixr 1  ?->, ?=>

(?->)   :: Probes a -> Property b -> Property (a -> b)
(?=>)   :: Probes a -> (a -> Property b) -> Property (a -> b)

probes ?-> prop  =  \ f -> and [ prop (f x)   | x <- probes ]
probes ?=> prop  =  \ f -> and [ prop x (f x) | x <- probes ]

--3.5.1
ordered      :: (Ord a) => Property [a]
ordered [] = True
ordered (x : []) = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

--3.5.2
swap :: Int -> Int -> [a] -> [a]
swap i j xs = if i == j then xs else left ++ [elemJ] ++ middle ++ [elemI] ++ right 
  where elemI = xs !! i
        elemJ = xs !! j
        left = take i xs
        middle = take (j - i - 1) (drop (i + 1) xs)
        right = drop (j + 1) xs

loop :: Int -> [a] -> Int -> Int -> Probes [a]
loop i list l r
 | i <= r = (permute (swap (l) (i) (list)) (l+1) (r))  ++  (loop (i+1) (list) (l) (r))
 | otherwise = [[]]

permute :: [a] -> Int -> Int -> Probes [a]
permute list l r 
 | l == r = [list]
 | otherwise =  loop (l) (list) (l) (r)

permutations :: [a] -> Probes [a] -- (length a)! nr of solutions
permutations list = filter (\x -> length x > 0) (permute (list) (0) ((length list)-1))

--3.5.3
sortingTheArray                 :: (a -> a -> Bool) -> [a] -> [[a]]
sortingTheArray rel []          =  []
sortingTheArray rel (x:xs)      =  (x:ys) : sortingTheArray rel zs
  where (ys,zs) = groupByAux x xs
        groupByAux x0 (x:xs) | rel x0 x = (x:ys, zs)
          where (ys,zs) = groupByAux x xs
        groupByAux y xs = ([], xs)


runs :: (Ord a) =>[a] -> [[a]]
runs a = sortingTheArray (<=) a

testRuns :: (Ord a) => [a] -> Bool
testRuns a = a == (concat ( sortingTheArray (<=) a))

--3.5.4
isqrt :: Integer -> Integer
isqrt n = loop 0 3 1
  where loop i k s  | s <= n      = loop (i + 1) (k + 2) (s + k)
                    | otherwise  = i

isIntegerSqrt :: Property(Integer -> Integer)
isIntegerSqrt f = and ( map ( \x -> x == f(x*x)) [1..10])

--3.5.5
infixr 4  <*>
(<*>) :: Probes a -> Probes b -> Probes (a, b)
(<*>) xs ys = [(x,y) | x <- xs, y <-ys]
 
niftySort :: [a] -> [a]
niftySort _xs  =  []

trustedSort :: (Ord a) => [a] -> [a]
trustedSort  =  sort
