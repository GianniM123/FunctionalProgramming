module DigitalSorting
where
import Data.List (groupBy, sortBy)

class Rank key where
  sort  ::  [(key, val)] -> [val]
  rank  ::  [(key, val)] -> [[val]]
  sort  =  concat . rank

genericSort :: (Ord key) => [(key, val)] -> [val]
genericSort kvs  = map snd (sortBy (\ kv1 kv2 -> compare (fst kv1) (fst kv2)) kvs)

instance Rank () where
  sort kvs   =  map snd kvs
  rank kvs   =  [ map snd kvs | not (null kvs) ]
  rank x = assoc x

-- instance (Rank key1, Rank key2) => Rank (key1, key2) where

-- instance (Rank key1, Rank key2) => Rank (Either key1 key2) where

type List elem  =  Either () (elem, [elem])

toList :: [elem] -> List elem
toList []        =  Left ()
toList (a : as)  =  Right (a, as)

-- instance (Rank key) => Rank [key] where

-- repeatedSegments :: (Rank key) => Int -> [key] -> [[Integer]]

-- instance Rank Base where
