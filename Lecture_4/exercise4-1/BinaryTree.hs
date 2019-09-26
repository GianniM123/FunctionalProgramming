module BinaryTree
where
import Data.List
data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
  deriving (Show)

instance Functor Tree where
  fmap _f Empty         =  Empty
  fmap f  (Node l a r)  =  Node (fmap f l) (f a) (fmap f r)

ex1  ::  Tree Integer
ex1  =  Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))
ex2  ::  Tree String
ex2  =  Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty
ex3  ::  Tree Char
ex3  =  Node (Node Empty 'a' Empty) 'k' (Node Empty 'z' Empty)

tree1 :: Tree Char
tree1 = Node (Node Empty 'a' (Node Empty 'b' Empty)) 'c' (Node (Node Empty 'd' Empty) 'f' (Node Empty 'g' Empty))
-- tree1 = Node (Node (Node Empty 'a' Empty) 'b' (Node Empty 'c' Empty)) 'd' (Node (Node Empty 'f' Empty) 'g' Empty)

{-
  4711
/       \
        0815
      /     \
              42
            /    \

            Ralf
           /     \     
        Peter
        /    \
      Frits
     /    \

    k
  /   \
  a    z 
/   \ / \
        
-}


size :: Tree elem -> Int
size (Empty ) = 0;
size (Node a _b c) = 1 + size (a) + size(c) 

minHeight, maxHeight :: Tree elem -> Int
minHeight (Empty) = 0
minHeight (Node a b c) =  if x < y then x else y where 
  x = 1 + minHeight a 
  y = 1 + minHeight c
maxHeight (Empty) = 0
maxHeight (Node a b c) =  if x > y then x else y where 
  x = 1 + maxHeight a 
  y = 1 + maxHeight c

--4.1.5 
-- size >= min+max height

member :: (Eq elem) => elem -> Tree elem -> Bool
member (_) (Empty) = False
member (x) (Node a b c) = b == x || member x a || member x c

 
preorder , inorder, postorder:: Tree elem -> [elem]
preorder (Empty) = []
preorder (Node a b c) = [b] ++ preorder a ++ preorder c
inorder (Empty) = []
inorder (Node a b c) =  inorder a ++ [b] ++ inorder c
postorder (Empty) = []
postorder (Node a b c) = postorder a ++ postorder c ++ [b]


showLayoutPart :: (Show elem) => Tree elem -> Int -> String
showLayoutPart (Empty) _ = ""
showLayoutPart (Node Empty a Empty) i = replicate i ' ' ++ "/ " ++ show a ++ "\n"
showLayoutPart (Node Empty b c) i = replicate i ' ' ++ "/ " ++ show b ++ (showLayoutPart2 c (i+4))  ++ "\n" 
showLayoutPart (Node a b Empty) i = (showLayoutPart a (i+4)) ++ replicate i ' ' ++ "/ " ++ show b  ++ "\n" 
showLayoutPart (Node a b c) i = (showLayoutPart a (i+4)) ++ replicate i ' ' ++ "/ " ++ show b ++ (showLayoutPart2 c (i+4))  ++ "\n" 

showLayoutPart2 :: (Show elem) => Tree elem -> Int -> String
showLayoutPart2 (Empty) _ = ""
showLayoutPart2 (Node Empty a Empty) i =  "\n" ++ replicate i ' ' ++ "\\ "++ show a
showLayoutPart2 (Node Empty b c) i = "\n" ++ replicate i ' ' ++ "\\ " ++ show b  ++ (showLayoutPart2 c (i+4))
showLayoutPart2 (Node a b Empty) i = "\n" ++ (showLayoutPart a (i+4)) ++ replicate i ' ' ++ "\\ " ++ show b ++ "\n"
showLayoutPart2 (Node a b c) i = "\n" ++ (showLayoutPart a (i+4))  ++ replicate i ' ' ++ "\\ " ++ show b ++  (showLayoutPart2 c (i+4))


layout :: (Show elem) => Tree elem -> String
layout (Node a b c) = showLayoutPart a 2 ++ "-" ++ show b ++ showLayoutPart2 c 2 ++ "\n"

balanced, build :: [elem] -> Tree elem
build [] = Empty
build (x:xs) = Node Empty (x) (build (xs)) 
balanced [] = Empty
balanced list = Node (build x) (head z) (build (tail z)) where (x,z) = splitAt (((length list) `div` 2)) (list)

-- create :: Int -> Tree ()
