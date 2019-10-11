module BinaryTree
where

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
  deriving (Show)

instance (Eq elem) => Eq (Tree elem) where
  Empty == Empty = True
  Node l1 e1 r1 == Node l2 e2 r2 = e1 == e2 && l1 == l2 && r1 == r2
  _ == _ = False

instance (Ord elem) => Ord(Tree elem) where
  Empty <= Empty = True
  Empty <= _ = True
  _ <= Empty = False
  Node l1 e1 r1 <= Node l2 e2 r2 = e1 <= e2 && l1 <= l2 && r1 <= r2


instance Functor Tree where
  fmap _f Empty         =  Empty
  fmap f  (Node l a r)  =  Node (fmap f l) (f a) (fmap f r)

ex1  ::  Tree Integer
ex1  =  Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))
ex2  ::  Tree String
ex2  =  Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty
ex3  ::  Tree Char
ex3  =  Node (Node Empty 'a' Empty) 'k' (Node Empty 'z' Empty)
ex4  ::  Tree Char
ex4  =  Node (Node Empty 'a' Empty) 'C' (Node Empty 'z' Empty)