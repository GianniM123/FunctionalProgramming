module Tutorial
where


allTrue :: (a-> Bool) -> [a] -> Bool
allTrue f l = foldr (\a r -> f a && r) True l


takeWhileTrue :: (a -> Bool) -> [a] -> [a]
takeWhileTrue f l= foldr (\a r -> if f a then a : r else r) [] l


factorial :: Int -> Int
factorial i = foldr (*) 1 [1..i]

sine :: Double -> Int -> Double
sine x lim = take lim (listComp x)


