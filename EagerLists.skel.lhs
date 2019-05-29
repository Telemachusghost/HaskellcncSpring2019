>	{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleContexts #-}

>	import Control.DeepSeq
>	import GHC.Generics (Generic, Generic1)

For reasons that I am unaware of, the NFData class refers to a type
that can be fully evaluated.

Ideas from http://users.aber.ac.uk/afc/stricthaskell.html#seq
Definition from http://hackage.haskell.org/package/deepseq-1.4.4.0/docs/Control-DeepSeq.html

Also consult Data.Strict.List, Control.DeepSeq
(Auto deriving an extension of standard Haskell)

>	data List a = Nil | Cons !a  !(List a)
>		deriving (Eq, Generic,NFData,Show)

The constructors are just injectors  -- they don't
need the strictness annotation.

rnf is the function to reduce to normal form
the above deriving is the same as the following:

	instance NFData (List a) where
		rnf Nil = seq Nil ()
		rnf (Cons x xs) = seq x $!! rnf xs 

>	instance Foldable (List) where
>		foldr = foldr'
>		foldl = foldl'	

Now the data structure is strict so can just write the following
methods as always.

>	maybeHead' :: List a -> Maybe a
>	maybeHead' Nil = Nothing
>	maybeHead' (Cons a _) = Just a

The above is really doing the following

	maybeHead (Cons x xs) = Just $ seq xs x

>	head' :: List t -> t
>	head' xs = case maybeHead' xs of
>	           Nothing -> error "Cannot take head of empty list"
>	           Just x  -> x

>	maybeTail' :: List t -> Maybe (List t)
>	maybeTail' Nil = Nothing
>	maybeTail' (Cons _ t) = Just t

Explicitly:
	maybeTail (Cons x xs) = Just $ seq x xs

>	tail' :: List t -> List t
>	tail' xs = case maybeTail' xs of
>	           Nothing -> error "Cant take a tail of empty list"
>	           Just t  -> t

>	drop' :: (Eq t1, Num t1) => t1 -> List t -> List t
>	drop' 0 xs = xs
>	drop' _ Nil = Nil
>	drop' n xs = drop' (n-1) (tail' xs) 

>	take' :: (Eq t, Num t) => t -> List a -> List a
>	take' _ Nil = Nil 
>	take' 0 xs  = Nil
>	take' n xs  = Cons (head' xs) (take' (n-1) (drop' 1 xs))

>	foldl' :: (t1 -> t -> t1) -> t1 -> List t -> t1
>	foldl' f i Nil = i
>	foldl' f i xs = (foldl' f (f i h) t) 
>	                where h = head' xs
>	                      t = tail' xs

>	foldr' :: (t1 -> t -> t) -> t -> List t1 -> t
>	foldr' f i Nil = i
>	foldr' f i xs  = f h (foldr' f i t) 
>	                 where h = head' xs
>	                       t = tail' xs    

>	append' :: List a -> List a -> List a
>	append' Nil ys = ys
>	append' xs ys = foldr' (\x y -> Cons x y) xs ys

>	reverse' :: List a -> List a
>	reverse' xs = foldl' (\acc x -> Cons x acc) Nil xs

Elements in range

>	range' :: (Num t, Eq t) => t -> t -> List a -> List a
>	range' n m xs = rangeList
>	                where
>	                prefixCut = drop' n xs
>	                rangeList = take' (m-n) prefixCut

Create a list within range

>	range'' :: (Ord a, Num a) => a -> a -> List a
>	range'' n m = case n < m of
>	              True  -> Cons n (range'' (n+1) m)
>	              False -> Cons m Nil

Eq is for free with Equality type!

>	map' :: NFData a => (t -> a) -> List t -> List a
>	map' f Nil = Nil 
>	map' f xs = foldr' (\x acc -> (Cons (f x) acc)) Nil xs

>	toList' :: List t -> [t]
>	toList' Nil = []
>	toList' xs = foldr' (\x acc -> x:acc) [] xs

>	concat' :: List (List a) -> List a
>	concat' xs = foldl' (\x acc -> append' acc x) Nil xs

I couldn't get List to be recognized as Foldable

	concatMap' :: Foldable t => (a -> [b]) -> t a -> [b]

>	concatMap' f xs = (concat . map' f) xs

>	cartProd' :: (NFData t1, NFData t) => t1 -> List t -> List (t1, t)
> 	cartProd' x ys = foldr' (\y acc -> (Cons (x,y) acc)) Nil ys

What was the intention of this function?

>	cartProd''
>	  :: (NFData t1, NFData t) =>
>	     List t1 -> List t -> List (List (t1, t))
>	cartProd'' xs _ = error "Not implemented yet"


>	cartProd
>	  :: (NFData t1, NFData t) =>
>	     List t1 -> List t -> List (List (t1, t))
>	cartProd xs ys = map' (\x -> cartProd' x ys) xs 

>	cartProdWith
>	  :: (NFData a, NFData t, NFData t1) =>
>	     ((t1, t) -> a) -> List t1 -> List t -> List a
>	cartProdWith f xs ys = concat' $ map' (\x -> foldF x ys) xs 
>	                       where
>	                       foldF x ys2 = foldr' (\y acc -> Cons (f (x,y)) acc) Nil ys2

	fromList :: (Foldable t, NFData a, NFData a2) => t a -> List a2

>	fromList xs = foldr (\x acc -> Cons x acc) Nil xs

>	l = [1..10]

>	l2 = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 Nil)))))))))

>	l3 = [1,2,undefined]

>	l4 = (Cons 1 (Cons 2 (Cons undefined Nil)))

>	l5 = fromList [1..10**6]

>	l6 = [1..10**6]

1. Demonstrate that this list is fully evaluated (use the literal undefined to populate the list with an undefined value).

*Main> take 1 l3
[1]
(0.00 secs, 58,536 bytes)
*Main> take' 1 l4
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries\base\GHC\Err.hs:78:14 in base:GHC.Err
  undefined, called at EagerLists.skel.lhs:153:36 in main:Main

2. Show the difference between this strict list and the lazy list.

Every term is evaluated to normal form as seen in the example in (1) taking 1 from l3 did not produce an error because the list does not evaluate to normal form.


3. Construct an example such as "head (map f xs)" which is slow for the strict list and fast for the lazy list and describe why this is so.

*Main> head' (map' (*2) l5)
2.0
(3.57 secs, 993,635,192 bytes)

*Main> head (map (*2) l6)
2.0
(0.08 secs, 60,048 bytes)

Our implementation actually evaluates the entire normal form i.e it applies *2 to all 10^6 elements, this uses a lot of memory and takes much longer.
The lazy implementation only evaluates what it needs to and just returns the head with the function applied.

