{-
Martijn Vogelaar & Gianni Monteban
s1047391 & s1047546
-}
module DigitalSorting
where
import DNA
import Data.List (groupBy, sortBy,tails,elemIndices)
import Data.Bifunctor


class Rank key where
  sort  ::  [(key, val)] -> [val]
  rank  ::  [(key, val)] -> [[val]]
  sort  =  concat . rank
  compare2 :: key -> key -> Ordering
  compare2 x y 
   | length (head.rank $[(x,1),(y,2)]) == 2= EQ
   | (head. sort $[(x,1),(y,2)]) == 1 = LT
   | (head. sort $[(x,1),(y,2)]) == 2 = GT

genericRank :: (Ord key) => [(key,val)] -> [[val]]
genericRank x =  map (\(z) -> map (\(_,y) -> y )(z) ) (groupBy (\(z,_) (y,_) -> z == y) $ sortBy (\(z,_) (y,_) -> compare z y) x)

genericSort :: (Ord key) => [(key, val)] -> [val]
genericSort kvs  = map snd (sortBy (\ kv1 kv2 -> compare (fst kv1) (fst kv2)) kvs)

instance Rank () where
  sort kvs   =  map snd kvs
  rank kvs   =  [ map snd kvs | not (null kvs) ]

assoc ((a,b),c) = (a,(b,c))

instance Rank Integer where
  rank = genericRank

instance (Rank key1, Rank key2) => Rank (key1, key2) where
  -- rank :: (Rank key1, Rank key2) => Rank [((key1, key2), val)] -> [[val]]
  rank x = concat $ map rank $ rank ( map (\y -> assoc y ) x )

instance (Rank key1, Rank key2) => Rank (Either key1 key2) where
  compare2 (Left a1) (Left a2) = compare2 a1 a2
  compare2 (Left a1) (Right a2) = LT
  compare2 (Right a1) (Left a2) = GT
  compare2 (Right b1) (Right b2) = compare2 b1 b2
  rank x =  map (\(z) -> map (\(_,y) -> y )(z) ) (groupBy (\(z,_) (y,_) -> compare2 z y == EQ) $ sortBy (\(z,_) (y,_) -> compare2 z y) x)

type List elem  =  Either () (elem, [elem])

toList :: [elem] -> List elem
toList []        =  Left ()
toList (a : as)  =  Right (a, as) 


instance (Rank key) => Rank [key] where
  rank x = rank [(toList a, b)| (a,b) <- x]

-- dummy implementation
rs x list =  map (\y -> elemIndices y a) b
  where a = filter (\y -> length y == x) . map (\y -> fst (splitAt x y)) $ tails list
        b = map (\(y:ys) -> y) . filter (\y -> length y > 1)  .groupBy (==) $ sortBy (compare) a


repeatedSegments :: (Rank key) => Int -> [key] -> [[Int]]
repeatedSegments x list = filter (\y -> length y > 1) $ rank (createTupleList 0 a)
  where a = filter (\y -> length y == x) . map (\y -> fst (splitAt x y)) $ tails list

createTupleList :: (Rank key) => Int -> [[key]] -> [([key],Int)]
createTupleList i [] = []
createTupleList i l = [(head l , i)] ++ createTupleList (i+1) (tail l)


instance Rank Base where
  rank = genericRank