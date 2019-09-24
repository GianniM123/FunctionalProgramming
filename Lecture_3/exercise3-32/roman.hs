-- Gianni Monteban & Martijn Vogelaar
-- s1047546 & s1047391

module Roman
where
import Data.List

data RD = M | D | C | L | X | V | I
    deriving (Eq)

data Roman = Roman [RD]

add :: Roman -> Roman -> Roman
add (Roman (x)) (Roman (y)) = Roman ((x ++ y))

number :: Roman
number = Roman ([M,M,C,C,C,X,X,I,I,I])

sortingTheArray                 :: (a -> a -> Bool) -> [a] -> [[a]]
sortingTheArray rel []          =  []
sortingTheArray rel (x:xs)      =  (x:ys) : sortingTheArray rel zs
  where (ys,zs) = groupByAux x xs
        groupByAux x0 (x:xs) | rel x0 x = (x:ys, zs)
          where (ys,zs) = groupByAux x xs
        groupByAux y xs = ([], xs)

rdtoInt :: RD -> Int
rdtoInt x
 | M == x = 1000
 | D == x = 500
 | C == x = 100
 | L == x = 50
 | X == x = 10
 | V == x = 5
 | I == x = 1
 | otherwise = error ("non exsisting roman")

intToRD :: Int -> ([RD], Int)
intToRD x
 | 1000 <= x = ([M], x-1000)
 | 900 <= x = ([C,M], x-900)
 | 500 <= x = ([D], x-500)
 | 400 <= x = ([C,D], x-400)
 | 100 <= x = ([C], x-100)
 | 90 <= x = ([X,C], x-90)
 | 50 <= x = ([L], x-50)
 | 40 <= x = ([X,L], x-40)
 | 10 <= x = ([X], x-10)
 | 9 <= x = ([I,X], x-9)
 | 5 <= x =  ([V], x-5)
 | 4 <= x = ([I,V], x-4)
 | 1 <= x = ([I], x-1)
 | otherwise = error ("non exsisting roman")

roman2int :: Roman -> Int
roman2int (Roman xs) = sum . concat. map (\x -> if length x == 2 then [x!!0 , (-1) * x!!1] else x) . groupBy (>) $ [ rdtoInt(x) | x<-(reverse xs)]

int2roman :: Int -> Roman
int2roman 0 = Roman([])
int2roman x = add (Roman(y)) (int2roman (z)) 
    where (y,z) = intToRD (x)
