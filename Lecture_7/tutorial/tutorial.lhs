> module Tutorial7
> where

== RULES ==
1: [] ++ bs = bs
2: (a:as) ++ bs = a : as ++ bs

3: insert a [] = [a]
4: insert a (b:bs)
     | a <= b = a : b : bs
     | otherwise = b : insert a bs

5: map f [] = []
6: map f (a:as) = f a : map f as

7: ordered [] = True
8: ordered [a] = True
9: ordered (a:b:cs) = a <= b && ordered (b:cs)

10: reverse [] = []
11: reverse (a:as) = reverse as ++ [a]

Let's start with some basic proofing stuff:
== Exercise 7.1 ==

Definition, definition bla bla bla.

Proof that:
ordered xs -> ordered (insert x xs)

Do we even have the ordered function?
Nah, so lets make it:

Also already under Rules

> ordered [] = True
> ordered [x] = True
> ordered (x:y:xs) = x <= y && ordered y:ys

Aight. So how do we do this?
Induction:

Base case:
	Suppose xs = []
	Assume ordered [] -- Because this is the left side of the implication.

	Therefore, proof:
	ordered (insert x [])
	=> ordered [x] -- 3
	=> True -- 8

Induction case:
	Suppose xs = (y:ys)
	IH: ordered (insert x ys)
	Assume ordered (y:ys) -- LHS of implication

	TP:
	ordered (insert x (y:ys))
		OH oh, now we have two cases:
	Suppose x <= y:
		ordered (x:y:ys)
		=> x <= y && ordered (y:ys) -- 9
		=> True && True -- assumption x <= y and LHS of implication
		=> True -- &&
	Suppose x > y:
		ordered (y : insert x ys)
		=> y <= head (insert x ys) && ordered (insert x ys) -- This is by
			definition of ordered. We know that the head will not fail, because
			the list will have at least x as an element.
		=> True && ordered (insert x ys) -- We know y <= head because y:ys was
		ordered and x > y.
		=> True && True -- IH
		=> True -- &&

Done!

I'm leaving ordered (insertionSort xs) for you to prove.

== Exercise 7.3 ==
How about we do this exercise next.

TP: map f (as ++ bs) = map f as ++ map f bs

What do we do induction on?
Two candidates: as and bs.
For as, the base case is going to be easy. For bs, not so much.
Because we get [] ++ as = as for free.

Base case:
	Assume as = []
	map f ([] ++ bs)
	=> map f bs -- 1
	=> [] ++ map f bs -- 1
	=> map f [] ++ map f bs -- 5

Induction step:
	Assume as = a:as
	IH: map f (as ++ bs) = map f as ++ map f bs
	map f ((a:as) ++ bs)
	=> map f (a : as ++ bs) -- 2
	=> f a : map f (as ++ bs) -- 6
	=> f a : map f as ++ map f bs -- IH
	=> (f a : map f as) ++ map f bs -- 2
	=> map f (a:as) ++ map f bs -- 6

DONE!

For fun, let's do another induction proof given the following function:
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
TP:
	reverse (xs ++ ys) = reverse ys ++ reverse xs
Base case:
	Assume xs = []
	reverse ([] ++ ys)
	=> reverse ys -- 1
	=> reverse ys ++ [] -- trivially proven (we can do this if you want)
	=> reverse ys ++ reverse [] -- 10
Induction case:
	Assume xs = x:xs
	IH: reverse (xs ++ ys) = reverse ys ++ reverse xs
	reverse ((x:xs) ++ ys)
	=> reverse (x : xs ++ ys) -- 2
	=> reverse (xs ++ ys) ++ [x] -- 11
	=> reverse ys ++ reverse xs ++ [x] -- IH
	=> reverse ys ++ reverse (x:xs) -- 11
DONE!

That was pretty cool.

This should put you in a position where you can make 7.4.

== Foldr Fusion ==
Let's prepare you for 7.6:
For those who have read the assignment, you would have noticed that we wanted
you to use something called the foldr fusion law. But what the hell
is this thing?

Let me just write the law down:
f (g a b) = h a (f b) -> f (foldr g e xs) = foldr h (f e) xs

Let us consider an example where we want to prove that
1 + sum [1 .. 10] is the same as sum [1, 1, .. 10]
First express sum as foldr:
sum xs = foldr (+) 0 xs
So:
	1 + foldr (+) 0 [1 .. 10] = foldr (+) 1 [1 .. 10]
If we write it in the form of the fusion law:
	(+1) (foldr (+) 0 [1 .. 10]) = foldr (+) ((+1) 0) [1 .. 10]
This holds, if we prove the condition of the implication. Where:
	f = (+1)
	g = +
	h = +
Combined:
	(+ 1) (a + b) ?= a + ((+1) b)
Yes, by law of associativity and transitivity.
Therefore:
1 + foldr (+) 0 [1 .. 10] = foldr (+) 1 [1 .. 10]

Alright, let do a harder example:
To prove: reverse . reverse = id
To fuse folds, we need folds.
Let's write reverse and id as folds:
	reverse xs = foldr (\b c -> c ++ [b]) [] xs
	id xs = foldr (\b c -> b : c) [] xs
Now, let's write the original statement with the folds:
	reverse (foldr (\b c -> c ++ [b]) [] xs) ?= foldr (\d e -> d : e) xs
Identify the f g and h:
	f = reverse
	g = \b c -> c ++ [b]
	h = \d e -> d : e
Proof that:
	reverse (b ++ [a]) = a : (reverse b)
This, we can prove:
	reverse (b ++ [a])
	=> reverse [a] ++ reverse b -- This is true, we have seen this earlier.
	=> [] ++ [a] ++ reverse b -- 11
	=> [a] ++ reverse b -- 1
	=> a : [] ++ reverse b -- 2
	=> a : reverse b -- 1

In short, proving using the foldr fusion law will typically follow the
following steps:
1. Write your supposed equality in the form of the RHS
2. Identify f, g, h
3. Prove the RHS

