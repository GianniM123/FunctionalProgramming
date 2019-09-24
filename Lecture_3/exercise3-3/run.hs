module Run
where

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