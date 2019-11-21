
module Generate
where
-- import Unicode

bools  ::  [Bool]
bools  =  pure False ++ pure True

maybes  ::  [elem] -> [Maybe elem]
maybes elems  =  pure Nothing ++ (pure Just <*> elems)

data Suit  =  Spades | Hearts | Diamonds | Clubs
data Rank  =  Faceless Integer | Jack | Queen | King
data Card  =  Card Rank Suit | Joker

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

cards :: [Card]
cards = error "cards: not yet implemented"

lists :: [elem] -> Int -> [[elem]]
lists es n = error "lists: not yet implemented"

trees :: [elem] -> Int -> [Tree elem]
trees es n = error "trees: not yet implemented"

-- lists bools 1
-- lists bools 2
-- trees (lists bools 2) 1
-- trees (lists bools 2) 2
