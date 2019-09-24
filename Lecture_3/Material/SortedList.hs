module SortedList
where
import Data.List

data SortList a = SL [a] deriving (Eq, Show)

newSortList   :: SortList a
newSortList = undefined
                           
memberSort    :: (Eq a, Ord a) => a -> SortList a -> Bool 
memberSort = undefined

insertSort    :: (Eq a, Ord a) => a -> SortList a -> SortList a       
insertSort = undefined

removeFirst   :: (Eq a) => a -> SortList a -> SortList a       
removeFirst = undefined

removeAll     :: (Eq a, Ord a) => a -> SortList a -> SortList a       
removeAll = undefined

elements      :: SortList a -> [a]                 
elements = undefined

count         :: SortList a -> Int                 
count = undefined

minimum       :: SortList a -> a                   
minimum = undefined

maximum       :: SortList a -> a                   
maximum = undefined

mergeSortList :: (Eq a, Ord a) => SortList a -> SortList a -> SortList a
mergeSortList = undefined

