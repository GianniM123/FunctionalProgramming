module BinarySearchTree
where
import BinaryTree  -- hiding (member)
import QuickTest

registry  ::  Tree String
registry  =  Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty

-- member :: (Ord elem) => elem -> Tree elem -> Bool
-- insert :: (Ord elem) => elem -> Tree elem -> Tree elem
-- delete :: (Ord elem) => elem -> Tree elem -> Tree elem

-- isSearchTree :: (Ord elem) => Tree elem -> Bool
-- trees :: [elem] -> Probes (Tree elem)  -- should be defined in BinaryTree
