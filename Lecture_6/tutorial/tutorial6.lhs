> module BinaryTree
> where

> import Hardware
> import Data.Monoid
> import Data.List

Note: This file can be loaded into GHCI without any effort. (Although you do
need Hardware.hs).

= Practical Session 6 =
== 6.1 ==
We have the following type:

> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

Our goal is now, to create two instances Eq, and Ord.
Start with Eq

> instance (Eq elem) => Eq (Tree elem)
>   where
>     Empty == Empty = True

This is, obviously, the easiest case, but what is the next case?

>     (Node l1 e1 r1) == (Node l2 e2 r2) = e1 == e2 && l1 == l2 && r1 == r2

And we must not forget:

>     _ == _ = False

Next, the Ord instance:

> instance (Ord elem) => Ord (Tree elem)
>   where
>     Empty <= Empty = True
>     Empty <= (Node _ _ _) = True
>     (Node _ _ _)  <= Empty = False
>     (Node l1 e1 r1)  <= (Node l2 e2 r2) = l1 <= l2 && e1 <= e2 && r1 <= r2

* We can combine the first to cases, how?
-- instance (Ord elem) => Eq (Ord elem)
--   where
--     (Node _ _ _)  <= Empty = False
--     (Node l1 e1 r1)  <= (Node l2 e2 r2) = l1 <= l2 && e1 <= e2 && r1 <= r2
--     _ <= _ = False

== 6.3 ==

Let first start with the definition of a Monoid.
Just an identity element with some associative binary operation.

A few very simple ones:
mempty = 0
mapply x y = x + y

mempty = 1
mapply x y = x * y

mempty = []
mapply x y = x ++ y

> reduce  ::  (Monoid m) => [m] -> m
> reduce  =  foldr (<>) mempty

> newtype OrdList elem = Ord [elem]

> instance (Ord elem) => Monoid (OrdList elem)
>   where
>     mempty = Ord []
>     mappend (Ord a) (Ord b) = Ord $ foldr insert a b

or: mappend (Ord a) (Ord b) = Ord $ merge a b

> merge :: Ord a => [a] -> [a] -> [a]
> merge (x:xs) (y:ys)
>     | x <= y = x : xs `merge` (y:ys)
>     | otherwise = y : x : xs `merge` ys
> merge y x = y ++ x

Can we create a sorting function?

> sort :: (Ord a) => [a] -> [a]
> sort = fromOrdList . reduce . map (\x -> Ord [x])

> fromOrdList :: OrdList a -> [a]
> fromOrdList (Ord l) = l

== 6.6 ==

Alright, so that was vague as hell. Let me tell you what we are doing in this
exercise.

- Suppose that we have the Rank instance for (,), Either and ().
- Suppose that we have ways of converting every data type to a combination of (,), Either and ().
- We then have Rank for every possible datatype.

Let practive with Lists.
data [] a = [] | (:) a [a]
Or, in terms of Either, (,) and ():
data List a = Either () (a,[a])

toList :: [a] -> List a
toList [] = Left ()
toList (a:as) = Right (a, as)
