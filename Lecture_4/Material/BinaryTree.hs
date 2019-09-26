module BinaryTree
where

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
  deriving (Show)

instance Functor Tree where
  fmap _f Empty         =  Empty
  fmap f  (Node l a r)  =  Node (fmap f l) (f a) (fmap f r)

ex1 :: Tree Integer
ex1 = Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))
ex2 :: Tree String
ex2 = Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty
ex3 :: Tree Char
ex3 = Node (Node Empty 'a' Empty) 'k' (Node Empty 'z' Empty)
ex4 :: Tree Char
ex4 = Node (Node Empty 'a' (Node Empty 'b' Empty)) 'c' (Node (Node Empty 'd' Empty )'f'(Node Empty 'g' Empty))

{-
    4711
   /    \
       0815
      /    \
           42
          /  \

------------------
          Ralf
         /    \
      Peter     
     /     \
   Frits 
  /     \

---------------------
       k
     /  \
    a   z
   / \ / \
-}

size :: Tree elem -> Int
size Empty = 0
size (Node l _ r) = 1 + size l + size r


minHeight :: Tree elem -> Int
minHeight Empty = 1
minHeight (Node l _ r) = 1 + (minHeight l) `min` (minHeight r)


maxHeight :: Tree elem -> Int
maxHeight Empty = 0
maxHeight (Node l _ r) = 1 + (maxHeight l) `max` (maxHeight r)

-- 4.1.5
-- Size is always larger or equal to both maxHeight or minHeight/?/

member :: (Eq elem) => elem -> Tree elem -> Bool
member _ Empty = False
member element (Node l e r) = e == element || member element l || member element r

preorder :: Tree elem -> [elem]
preorder Empty = []
preorder (Node l e r) = [e] ++ preorder l ++ preorder r

inorder :: Tree elem -> [elem]
inorder Empty = []
inorder (Node l e r) = inorder l ++ [e]  ++ inorder r


postorder :: Tree elem -> [elem]
postorder Empty = []
postorder (Node l e r) =  postorder l ++ postorder r ++ [e]

--4.2.1 Running time is O(n)


indent :: Int -> String
indent lengthLeft = replicate lengthLeft ' '
--"   " ++ (indent (lengthLeft-1))

printLayoutLeft :: (Show elem) => Tree elem -> Int -> [String]
printLayoutLeft Empty _ = []
printLayoutLeft (Node l n r) layer = (printLayoutLeft l (layer+1)) ++ [(indent (3*layer)) ++ "/" ++ " " ++ show n] ++ (printLayoutRight r (layer+1))


printLayoutRight :: (Show elem) => Tree elem -> Int -> [String]
printLayoutRight Empty _ = []
printLayoutRight (Node l n r) layer = (printLayoutLeft l (layer+1)) ++ [(indent (3*layer)) ++ "\\" ++ " " ++ show n] ++ (printLayoutRight r (layer+1))


printLayout :: (Show elem) => Tree elem -> [String]
printLayout Empty = []
printLayout (Node l n r) = (printLayoutLeft l 1) ++ ["- " ++ show n] ++ (printLayoutRight r 1)

layout :: (Show elem) => Tree elem -> String
layout tree = unlines (printLayout tree)
-- build :: [elem] -> Tree elem
-- balanced :: [elem] -> Tree elem
-- create :: Int -> Tree ()
