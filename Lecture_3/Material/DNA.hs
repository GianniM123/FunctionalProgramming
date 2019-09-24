-- Gianni Monteban & Martijn Vogelaar
-- s1047546 & s1047391

module DNA
where
import Data.List hiding (filter)
import Prelude hiding (filter)
import List

-- Nucleobases or DNA-bases are the basic building blocks of
-- deoxyribonucleic acid (DNA).

data Base  =  A | C | G | T
  deriving (Eq, Ord)

-- Adenin (A), Cytosin (C), Guanin (G) und Thymin (T).

instance Show Base where
  showsPrec _ A  =  showChar 'A'
  showsPrec _ C  =  showChar 'C'
  showsPrec _ G  =  showChar 'G'
  showsPrec _ T  =  showChar 'T'

  showList  =  foldr (.) id . map shows

base :: Char -> Maybe Base
base 'A'  =  Just A
base 'C'  =  Just C
base 'G'  =  Just G
base 'T'  =  Just T
base _    =  Nothing

type DNA      =  [Base]
type Segment  =  [Base]

dna  ::  DNA
dna  =  [A, T, G, T, A, A, A, G, G, G, T, C, C, A, A, T, G, A]

mm  ::  DNA
mm  =  filter base
   "ATGTAAAGGGTCCAATGACTGGAATTACTTCACAGCCCTGACACTGTGGAGAGATGGATA\
   \CCAGTTTAATTATGGACAGCTGAGAATAATCTGGGAGTCACCTGCTCTGGATATTTATCA\
   \TCATTATGAACTTCTCACCAGGCTGTGGCCCGAACACTTTCATAACGTCCCATTTGTGTT\
   \GGGCAGACATTATGATCTATACAGAACCTGCCTTGTCTGTCACCTTTGAATTTATGGATA\
   \GACAATTTAATAT\
   \GTGTTCCTGGCAGCAAAGATAATCATGGAGAGTGGAGAGAAACTAACCTTACCATTGATA\
   \GGGAAACTCTTGAAGTGTCAACTTCTCCATATTAAATCCAAGGACCAGCAGAGACGAGAA\
   \AATGAAAAGAAGATGGTAGAAGAAAGAACAAAATCAGAAAAAGACAAAGGAAAAGGGAAG\
   \TCTCCAAAGGAGAAGAAAGTTGCCAGTGCCAAGCCTGGGAAGGGAAAGAGCAAGGACCAG"

readDNA :: FilePath -> IO [Base]
readDNA path
  =  do  x<- readFile path
         return (filter base x)


  
contains            :: Segment -> DNA -> Bool
contains segment = elem (segment) . (map (take (length segment)) . tails )
-- contains segment dna = isInfixOf (segment) (dna)

-- searchPlace :: Int -> [Bool] -> [Int]
-- searchPlace _ [] = []
-- searchPlace c (x : xs)
--  | True == x = [(c)] ++ searchPlace (c+1) (xs)
--  |otherwise = searchPlace (c+1) (xs)

containsPlace :: Segment -> DNA -> [Int]
containsPlace segment = elemIndices (segment) . (map (take (length segment)) . tails )
-- containsPlace segment = searchPlace (0) . map (\x -> x == segment) . (map (take (length segment)) . tails )

ownFilter p xs = [ x | x <- xs, p x]

longestOnlyAs       :: DNA -> Integer
longestOnlyAs dna = toInteger .maximum . map (\x -> length x). ownFilter (\x -> contains ([A]) (x)) . sort . group $ dna


longestAtMostTenAs  :: DNA -> Integer
longestAtMostTenAs dna = toInteger .maximum . ownFilter (\x -> x <=10) .map (\x -> length x). ownFilter (\x -> contains ([A]) (x)) . sort . group $ dna

-- If you want to test your code on a larger example, say within GHCi

-- dna <- readDNA "mm1.dna"
-- longestOnlyAs dna
