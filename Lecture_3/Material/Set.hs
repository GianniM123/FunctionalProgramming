module Set
where
import Data.List

data Set a = Set [a] deriving (Show, Eq)

-- toSet			    :: Eq a => [a]           -> Set a 
-- fromSet			  :: Eq a => Set a         -> [a]

-- isEmptySet		  :: Eq a => Set a         -> Bool
-- isDisjoint		  :: Eq a => Set a -> Set a -> Bool 
-- isSubset		    :: Eq a => Set a -> Set a -> Bool 
-- isStrictSubset	:: Eq a => Set a -> Set a -> Bool 
-- memberOfSet		:: Eq a => a     -> Set a -> Bool 
-- union          :: Eq a => Set a -> Set a -> Set a
-- intersection	  :: Eq a => Set a -> Set a -> Set a
-- nrOfElements	  :: Eq a => Set a -> Int
-- without			  :: Eq a => Set a -> Set a -> Set a 
-- product			  :: Eq a => Set a Set b -> Set (a,b)

-- powerSet		    :: Eq a => (Set a)         -> Set (Set a)