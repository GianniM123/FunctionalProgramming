module StdAssocList
where

type AssocList k a = [(k,a)]

newAssocList :: AssocList k a
newAssocList = error "newAssocList not yet implemented"

countValues :: AssocList k a -> Int
countValues = error "countValues not yet implemented"

lookupKey :: (Eq k, Ord k) => k -> AssocList k a -> [a]
lookupKey = error "lookupKey not yet implemented" []

updateKey :: (Eq k, Ord k) => k ->  a -> AssocList k a -> AssocList k a
updateKey = error "updateKey not yet implemented" []

removeKey :: (Eq k, Ord k) => k ->  AssocList k a -> AssocList k a
removeKey = error "removeKey not yet implemented" []
