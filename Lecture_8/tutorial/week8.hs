import Data.Array
import Prelude hiding (head,tail,repeat,(&&),zip,zipWith,null)

-----------------------------------------------------------------

shorter :: [a] -> [b] -> Bool
shorter xs ys = length xs < length ys

ones = 1 : ones

q1 = shorter [1,2] [3,4,5]

q2 = shorter [1,2] ones

q4 = False && shorter [1,2] ones
              ------------------ undefined

(&&) :: Bool -> Bool -> Bool
False && _ = False
True && x = x

shorter' :: a] -> [b] -> Bool
shorter' _ [] = False
shorter' [] _ = True
shorter' (_:xs) (_:ys) = shorter' xs ys

-----------------------------------------------------------------

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

{-
fib 5 = fib 4 + fib 3
      = (fib 3 + fib 2) + (fib 2 + fib 1)
      = ((fib 2 + fib 1) + (fib 1 + fib 0)) + ((fib 1 + fib 0) + 1)
      = 
-}

----------------

fibs1 :: [Int]
fibs1 = [ fib' n | n <- [0..] ]

fib' 0 = 0
fib' 1 = 1
fib' n = fibs !! (n-1) + fibs !! (n-2)

-- complexity of fib n?
-- 

---------------

zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

zipWith f [] _ = []
zipWith f _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

{-
fibs = 0 : 1 : 1 : (1+1) : zipWith (+) (1:1:..) (1:...)
-}

-------------------------------------------------------------------

takeWhile p [] = []
takeWhile p (x:xs)
 | p x = x : takeWhile p xs
 | otherwise = []

null [] = True
null (_:_) = False

primes = [ x | x <- [2..], isPrime x ]
isPrime x = null [ p | p <- takeWhile (<x) primes, x `mod` p == 0 ]

{-
primes = ???
isPrime 2 = null [ ..  ]
takeWhile (<2) primes = _|_
-}

-------

primes = 2 : [ x | x <- [3,5..], isPrime x ]
isPrime x = null [ p | p <- takeWhile (<x) primes, x `mod` p == 0 ]
--isPrime x = not (any (\p -> x `mod` p == 0) (takeWhile (<x) primes))

{-
primes = 2 : ???
isPrime 3 = null [ .. ]
takeWhile (<3) primes = 2 : takeWhile (<3) ???
-}

-------

primes = 2 : [ x | x <- [3,5..], isPrime x ]
isPrime x = null [ p | p <- takeWhile (\p -> p^2 <= x) primes, x `mod` p == 0 ]

{-
primes = 2 : 3 : 5 : 7 : 
isPrime 3 = True
takeWhile (\p -> p^2 <= 3) primes = []


isPrime 5 = True
takeWhile (\p -> p^2 <= 5) primes = 2 : []

isPrime 9 = False
takeWhile (\p -> p^2 <= 9) primes = 2 : 3 : []
-}

-------------------------------------------------------------------

data Stream elem = Cons { head :: elem, tail :: Stream elem }

repeat :: elem -> Stream elem
repeat = undefined

instance Num elem => Num (Stream elem) where
  fromInteger x = repeat (fromInteger elem)

---------

codes :: Tree char -> [(char, [Bit])]
codes (Leaf x) = [_something]
codes (x :^: y) = [ _something_here | (char,bits) <- codes x]

---------------------------------------------------------------

{-

(!!) :: [a] -> Int -> a
(!) :: Ix ix => Array ix a -> ix -> a

class Ix ix
instance Ix Int
instance (Ix a, Ix b) => Ix (a,b)

-}


stamps :: [Int] -> Int -> Int
stamps available toMake = theChosenStamps ! toMake
  where
  theChosenStamps = listArray (0,toMake) [stamps' i | i <- range (0,toMake)]
  stamps' n
     | n == 0 = 0
     | otherwise = minimum [ (theChosenStamps ! (n - x)) + 1 | x <- available, x <= n ]

---------------------------------

{-
Contact info:

Twan van Laarhoven
tvanlaarhoven@cs.ru.nl
-}

