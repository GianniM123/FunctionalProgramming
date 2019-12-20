{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Minimax2
where
import Squiggol

data Tree elem  =  Node elem [Tree elem]

data TREE elem tr = NODE elem [tr] 


t = Node "S" [Node "a" [Node "c" []], Node "b" []]

instance Functor (TREE elem) where
    fmap f (NODE elem rs) = NODE elem (map f rs)

instance Base (TREE elem) where
    type Rec (TREE elem) = Tree elem
    inn (NODE elem rs) = Node elem rs
    out (Node elem rs) = NODE elem rs

size, depth :: Tree elem -> Integer
size = fold $ \(NODE e t) -> sum t + 1
depth = fold f
    where f (NODE e []) = 1
          f (NODE e t) = 1 + maximum t

gametree :: (position -> [position]) -> (position -> Tree position)
gametree f = unfold (\position -> NODE position (f position))

winning  :: Tree position -> Bool
winning _ = True