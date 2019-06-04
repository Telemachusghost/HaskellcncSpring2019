> import Control.Applicative

> data List a = Empty | Cons a (List a)
>         deriving (Show, Eq)

1. concatMap maps a function that produces a list over 
another list, and then concatenates them all together. Here are 2
definitions:

Here are some helper functions taken from EagerList hw

> maybeHead' :: List a -> Maybe a
> maybeHead' Empty = Nothing
> maybeHead' (Cons a _) = Just a

> head' :: List t -> t
> head' xs = case maybeHead' xs of
>            Nothing -> error "Cannot take head of empty list"
>            Just x  -> x

> maybeTail' :: List t -> Maybe (List t)
> maybeTail' Empty = Nothing
> maybeTail' (Cons _ t) = Just t

> tail' :: List t -> List t
> tail' xs = case maybeTail' xs of
>            Nothing -> error "Cant take a tail of empty list"
>            Just t  -> t

> foldr' :: (t1 -> t -> t) -> t -> List t1 -> t
> foldr' f i Empty = i
> foldr' f i xs  = f h (foldr' f i t) 
>                  where 
>                  h = head' xs
>                  t = tail' xs

> foldl' :: (t1 -> t -> t1) -> t1 -> List t -> t1
> foldl' f i Empty = i
> foldl' f i xs = (foldl' f (f i h) t) 
>                 where h = head' xs
>                       t = tail' xs


> map' :: (t -> a) -> List t -> List a
> map' f Empty = Empty 
> map' f xs = foldr' (\x acc -> (Cons (f x) acc)) Empty xs  

> concat' :: List (List a) -> List a
> concat' xs = foldl' (\x acc -> append' x acc) Empty xs


> append' :: List a -> List a -> List a
> append' Empty ys = ys
> append' xs ys = foldr' (\x y -> Cons x y) ys xs


ConcatMapL Function

> concatMapL :: (t -> List a) -> List t -> List a
> concatMapL f xs = (concat' . map' f) xs 

enumFromToL function

> enumFromToL :: (Ord a, Num a) => a -> a -> List a
> enumFromToL x y | x == y = Cons x Empty
>                 | x < y  = Cons x (enumFromToL (x+1) y)
>                 | x > y  = error "From var is greater than To var"

Testing for concatMapL and enumFromToL

*Main> concatMapL (enumFromToL 1) (Cons 1 (Cons 2 (Cons 3 Empty)))
Cons 1 (Cons 1 (Cons 2 (Cons 1 (Cons 2 (Cons 3 Empty)))))
(0.01 secs, 113,272 bytes)

Write appendL that appends two lists

> appendL :: List a -> List a -> List a
> appendL Empty ys = ys
> appendL xs ys = foldr' (\x y -> Cons x y) ys xs

> listComp :: List (t -> a) -> List t -> List a
> listComp fs xs = foldr' (\x acc -> appendL (map' x xs) acc) Empty fs


2. Make the List an instance of Functor (recall a Functor just
needs to have fmap defined). Notice that you have to put in the literate
character since this is a file that can be loaded into ghci directly
and I couldn't just put in a dummy expression.

> instance Functor List where
>          fmap = map'

> instance Applicative List where
>          pure x = Cons x Empty
>          Empty <*> _ = Empty
>          fs <*> xs = listComp fs xs   

Test

*Main> fmap (+1) (Cons 1 (Cons 2 (Cons 3 Empty)))
Cons 2 (Cons 3 (Cons 4 Empty))
(0.01 secs, 84,296 bytes)

> instance Foldable List where
>          foldr = foldr'
>          foldl = foldl'

Testing

*Main> foldr (\x y -> x+y) 0 (Cons 1 (Cons 2 (Cons 3 Empty)))
6
(0.01 secs, 59,568 bytes)

*Main> foldl (\x y -> x+y) 0 (Cons 1 (Cons 2 (Cons 3 Empty)))
6
(0.01 secs, 58,832 bytes)

*Main> length (Cons 1 (Cons 2 (Cons 3 Empty)))
3
(0.00 secs, 58,224 bytes)

3. I already did Applicative List above

> instance Monad List where
>         return x     = pure x
>         Empty >>= _  = Empty
>         list >>= f   = concatMapL f list  

4. Write the following functions

> pairs [] ys = []
> pairs xs [] = []
> pairs (x:xs) ys = pairup x ys ++ pairs xs ys
>                   where
>                   pairup v [] = []
>                   pairup v (x:xs) = (v,x):(pairup v xs)

> pairs1 :: [Int] -> [Int] -> [(Int, Int)]
> pairs1 xs ys = 
>        xs >>= (\x ->
>         ys >>= (\y ->
>          [(x,y)]))

> pairs' Empty ys = Empty
> pairs' xs Empty = Empty
> pairs' (Cons x xs) ys = pairup' x ys `appendL` pairs' xs ys
>                        where
>                        pairup' v Empty = Empty
>                        pairup' v (Cons x xs) = (Cons (v,x) (pairup' v xs))  

Test
*Main> pairs' (Cons 1 (Cons 2 Empty)) (Cons 3 (Cons 1 Empty))
Cons (1,3) (Cons (1,1) (Cons (2,3) (Cons (2,1) Empty)))
(0.01 secs, 105,008 bytes)

> pairs1' :: Monad m => m a -> m b -> m (a, b)
> pairs1' xs ys =
>         xs >>= (\x ->
>          ys >>= (\y ->
>          return (x,y)))

Test
*Main> pairs1' (Cons 1 (Cons 2 Empty)) (Cons 3 (Cons 1 Empty))
Cons (1,3) (Cons (1,1) (Cons (2,3) (Cons (2,1) Empty)))
(0.01 secs, 112,496 bytes)

> pairs2' :: Monad m => m a -> m b -> m (a, b)
> pairs2' xs ys = do
>                 x <- xs
>                 y <- ys
>                 return (x,y) 

Test
*Main> pairs2' (Cons 1 (Cons 2 Empty)) (Cons 3 (Cons 1 Empty))
Cons (1,3) (Cons (1,1) (Cons (2,3) (Cons (2,1) Empty)))
(0.01 secs, 112,616 bytes)

> mapL' :: (t -> a) -> List t -> List a
> mapL' f xs = (pure f) `listComp` xs

Test
*Main> mapL' (**2) (Cons 2 (Cons 4 Empty))
Cons 4.0 (Cons 16.0 Empty)
(0.01 secs, 130,896 bytes)

> pairs3' :: List t1 -> List t2 -> List (t1, t2)
> pairs3' xs ys = fs `listComp` ys
>                 where
>                 fs = fmap (\x y -> (x,y)) xs

Test
*Main> pairs3' (Cons 1 (Cons 2 Empty)) (Cons 3 (Cons 1 Empty))
Cons (1,3) (Cons (1,1) (Cons (2,3) (Cons (2,1) Empty)))
(0.01 secs, 108,928 bytes)

> filterL :: (a -> Bool) -> List a -> List a
> filterL p xs = do
>                x <- xs
>                if p x then pure x else Empty

Test
*Main> filterL (<2) (Cons 1 (Cons 4 Empty))
Cons 1 Empty
(0.01 secs, 70,784 bytes)
*Main> filterL (<2) (Cons 2 (Cons 4 Empty))
Empty
(0.01 secs, 63,912 bytes)

> pairs4' :: Ord a => List a -> List a -> List (a,a)
> pairs4' xs ys = do 
>                 x <- xs
>                 y <- ys
>                 if x < y then pure (x,y) else Empty

Test
*Main> pairs4' (Cons 1 (Cons 2 Empty)) (Cons 3 (Cons 1 Empty))
Cons (1,3) (Cons (2,3) Empty)
(0.01 secs, 91,400 bytes)

5. An alternative way to filter out only the pairs we want is
to use the Alternative typeclass. I am providing the instance
declaration for the Alternative typeclass and the definition of
guard.

> instance Alternative List where
>         empty = Empty
>         (<|>) = appendL

> guard :: Alternative m => Bool -> m ()
> guard True = pure ()
> guard _    = empty

> filterL' :: (Monad m, Alternative m) => (b -> Bool) -> m b -> m b
> filterL' p xs = do
>                 x <- xs
>                 guard(p x) 
>                 return x

Test

*Main> filterL' (<2) (Cons 2 (Cons 4 Empty))
Empty
(0.01 secs, 65,392 bytes)
*Main> filterL' (<2) (Cons 1 (Cons 4 Empty))
Cons 1 Empty
(0.01 secs, 72,688 bytes)