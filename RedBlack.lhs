> data Color = R | B deriving (Show)

All empty nodes are black

> data Tree a = E | T Color (Tree a) a (Tree a) deriving (Show)

> empty = E

> mem :: Ord a => a -> Tree a -> Bool
> mem x E = False
> mem x (T _ tl elem tr) = if x < elem then mem x tl else if x > elem then mem x tr else True

Before we have insert we need a balance function
Refactor to have better names for variables

> balance t = case t of
>             (T B (T R (T R a x b) y c) z d) -> (T R (T B a x b) y (T B c z d)) -- Left Left Case
>             (T B (T R a x (T R b y c)) z d) -> (T R (T B a x b) y (T B c z d)) -- Left Right case
>             (T B a q (T R b x (T R f r t))) -> (T B (T R a q b) x (T R f r t)) -- Right Right case
>             (T B a x (T R (T R y z t) p r)) -> (T B (T R a x y) z (T R y p r)) -- Right Left case
>             b                               -> b  

Insert function that uses balance


> insert x t = let (T _ a y b) = ins t in (T B a y b)
>              where
>                   ins t = case t of
>                           E -> (T R E x E)
>                           t1@(T c a y b) ->  if x < y then balance (T c (ins a) y b) 
>                                              else if x > y then balance (T c a y (ins b))
>                                              else t1

What difference does the list being sorted make? Is it because it makes insertion slower?

> fromOrdList list = foldl (\acc x -> insert x acc) empty list  

