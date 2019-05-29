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

Now the data structure is strict so can just write the following
methods as always.

>	maybeHead' :: List a -> Maybe a
>	maybeHead' Nil = Nothing
>		-- TODO

The above is really doing the following

	maybeHead (Cons x xs) = Just $ seq xs x

>	head' :: List t -> t
>	head' xs = error "Not implemented yet"
>		-- TODO

>	maybeTail' :: List t -> Maybe (List t)
>	maybeTail' Nil = Nothing
>		-- TODO

Explicitly:
	maybeTail (Cons x xs) = Just $ seq x xs

>	tail' :: List t -> List t
>	tail' xs = error "Not implemented yet"
>		-- TODO

>	drop' :: (Eq t1, Num t1) => t1 -> List t -> List t
>	drop' n xs = error "Not implemented yet"
>		-- TODO

>	take' :: (Eq t, Num t) => t -> List a -> List a
>	take' n xs = error "Not implemented yet"
>		-- TODO

>	foldl' :: (t1 -> t -> t1) -> t1 -> List t -> t1
>	foldl' f i xs = error "Not implemented yet"
>		-- TODO

>	foldr' :: (t1 -> t -> t) -> t -> List t1 -> t
>	foldr' f i xs = error "Not implemented yet"
>		-- TODO

>	append' :: List a -> List a -> List a
>	append' xs ys = error "Not implemented yet"
>		-- TODO

>	reverse' :: List a -> List a
>	reverse' xs = error "Not implemented yet"
>		-- TODO

>	range' :: (Num t, Eq t) => t -> t -> List a -> List a
>	range' n m xs = error "Not implemented yet"
>		-- TODO

>	range'' :: (Ord a, Num a) => a -> a -> List a
>	range'' n m = error "Not implemented yet"
>		-- TODO

Eq is for free with Equality type!

>	map' :: NFData a => (t -> a) -> List t -> List a
>	map' f xs = error "Not implemented yet"
>		-- TODO

>	toList' :: List t -> [t]
>	toList' xs = error "Not implemented yet"
>		-- TODO

>	concat' :: List (List a) -> List a
>	concat' xs = error "Not implemented yet"
>		-- TODO

>	concatMap' :: Foldable t => (a -> [b]) -> t a -> [b]
>	concatMap' f xs = error "Not implemented yet"
>		-- TODO

>	cartProd' :: (NFData t1, NFData t) => t1 -> List t -> List (t1, t)
> 	cartProd' x ys = error "Not implemented yet"
>		-- TODO

>	cartProd''
>	  :: (NFData t1, NFData t) =>
>	     List t1 -> List t -> List (List (t1, t))
>	cartProd'' xs _ = error "Not implemented yet"


>	cartProd
>	  :: (NFData t1, NFData t) =>
>	     List t1 -> List t -> List (List (t1, t))
>	cartProd xs ys = error "Not implemented yet"

>	cartProdWith
>	  :: (NFData a, NFData t, NFData t1) =>
>	     ((t1, t) -> a) -> List t1 -> List t -> List a
>	cartProdWith f xs ys = error "Not implemented yet"
