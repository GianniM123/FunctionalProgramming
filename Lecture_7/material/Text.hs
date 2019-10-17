module Text
where
import BinaryTree

-- Interface.

infixr 5 <++> 
text    :: String -> Text       -- without '\n'
nl      :: Text
indent  :: Int -> Text -> Text
(<++>)    :: Text -> Text -> Text

render  :: Text -> String

-- Reference implementation.

data Text  =  Text String      -- without '\n'
           |  Nl
           |  Indent Int Text
           |  Text :++ Text
           deriving (Show)

text    =  Text
nl      =  Nl
indent  =  Indent
(<++>)  =  (:++)

render (Text s)      =  s
render (Nl)          =  "\n"
render (Indent i d)  =  tab i (render d)
render (d1 :++ d2)   =  render d1 ++ render d2

-- The helper function ``tab i s'' inserts ``i'' spaces after each
-- newline in ``s''.

tab :: Int -> String -> String
tab _i ""        =  ""
tab i (c : s)
    | c == '\n'  =  c : replicate i ' ' ++ tab i s
    | otherwise  =  c :                    tab i s

-- Application.

prettyTree :: (Show elem) => Tree elem -> Text
prettyTree Empty
  =  text "Empty"
prettyTree (Node l a r)
  =  indent 4 (  text "Node"    <++> nl <++>
                 prettyTree l   <++> nl <++>
                 text (show a)  <++> nl <++>
                 prettyTree r)

atree :: Tree Integer
atree
  = Node (Node Empty 1 (Node Empty 2 Empty))
       3 (Node (Node Empty 4 Empty) 5 (Node Empty 6 Empty))

-- prettyTree atree
-- render it
-- putStrLn it
