
module Huffman
where
import Satellite
import Tree
import Data.List
import Data.Maybe
-- -------------------------------------------------------------------------------

-- Warm-up: constructing a frequency table.

frequencies  ::  (Ord char) => [char] -> [With Int char]
frequencies xs = sort [ length x :- head x| x <- (group . sort $  xs)]

-- -------------------------------------------------------------------------------

-- Constructing a Huffman tree.
huffmanCreation (x : []) = [x]
huffmanCreation (x1 : x2 : xs) = huffmanCreation . sort $ (((rocket x1 + rocket x2) :- (satellite x1 :^: satellite x2)) : xs)

huffman :: [With Int char] -> Tree char
huffman xs = satellite .head . huffmanCreation $ list
  where list = [rocket x :- Leaf (satellite x) | x <- xs] 
-- -------------------------------------------------------------------------------


-- Encoding ASCII text.

data Bit = O | I
  deriving (Show, Eq, Ord)


subCodes :: Tree char -> [Bit] -> [(char, [Bit])]
subCodes (Leaf x) z = [(x, z)] 
subCodes (x :^: y) z = subCodes (x) (z ++ [O]) ++ subCodes (y) (z ++ [I])

codes :: Tree char -> [(char, [Bit])]
codes tannenbaum = subCodes tannenbaum []

searchForBitjes :: (Eq char) => [(char,[Bit])] -> char -> [Bit]
searchForBitjes [] _ = []
searchForBitjes (x : xs) c = if fst x == c then snd x else searchForBitjes xs c

encode :: (Eq char) => Tree char -> [char] -> [Bit]
encode tannenbaum woordje = concat (map (\x -> searchForBitjes bitjes x) woordje)
  where bitjes = codes tannenbaum



-- -------------------------------------------------------------------------------

-- Decoding a Huffman binary.
searchForLetter :: [(char,[Bit])] -> [Bit] -> Maybe char
searchForLetter [] _ = Nothing
searchForLetter (x : xs) c = if snd x == c then Just (fst x) else searchForLetter xs c

subDecode :: Tree char -> [Bit] -> Int -> [char]
subDecode tannenbaum [] i = []
subDecode tannenbaum b i = if isNothing found then subDecode tannenbaum b (i+1) else [fromJust found] ++ subDecode tannenbaum (drop i b) (0)
  where found = searchForLetter (codes tannenbaum) (take i b)

decode :: Tree char -> [Bit] -> [char]
decode tannenbaum b = subDecode tannenbaum b 0
-- -------------------------------------------------------------------------------

-- Some test data.

ex82 :: [With Int Char]
ex82 = [74 :- 'z',95 :- 'q',150 :- 'x',153 :- 'j',772 :- 'k',978 :- 'v',1492 :- 'b',1929 :- 'p',1974 :- 'y',2015 :- 'g',2228 :- 'f',2360 :- 'w',2406 :- 'm',2758 :- 'c',4025 :- 'l',4253 :- 'd',5987 :- 'r',6094 :- 'h',6327 :- 's',6749 :- 'n',6966 :- 'i',7507 :- 'o',8167 :- 'a',9056 :- 't',12702 :- 'e']

hw, why :: String
hw =
  "hello world"


identity :: [Char] -> [Char]
identity x =  decode ct (encode ct x)
  where ct = (huffman (frequencies x))

ct = huffman (frequencies hw)
-- code = huffman (frequencies hw)
-- encode code hw
-- decode code it
-- decode code it == hw

why =
  "As software becomes more and more complex, it\n\
  \is  more  and  more important to structure it\n\
  \well.  Well-structured  software  is  easy to\n\
  \write,   easy   to   debug,  and  provides  a\n\
  \collection  of modules that can be re-used to\n\
  \reduce future programming costs. Conventional\n\
  \languages place a conceptual limit on the way\n\
  \problems   can   be  modularised.  Functional\n\ 
  \languages  push  those  limits  back. In this\n\
  \paper we show that two features of functional\n\
  \languages    in    particular,   higher-order\n\
  \functions and lazy evaluation, can contribute\n\
  \greatly  to  modularity.  Since modularity is\n\
  \the key to successful programming, functional\n\
  \languages  are  vitally important to the real\n\
  \world."

-- code = huffman (frequencies why)
-- encode code why
-- decode code it
-- decode code it == why
