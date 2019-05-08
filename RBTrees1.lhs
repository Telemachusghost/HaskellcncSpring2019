>	module RBTrees1 where

>	import RedBlackTrees 

See if Haskell has something comparable to Debug in ELM
import Debug

>	insert :: Int -> Tree -> Tree
>	insert x t = T B l y r
>		where
>			(T _ l y r) = ins x t

>	ins :: Int -> Tree -> Tree
>	ins x t = case t of
>	          E -> (T R E x E)
>	          t1@(T c a y b) ->  if x < y then balanceL c (ins x a) y b 
>                                else if x > y then balanceR c a y (ins x b)
>                                else t1

>	balanceL :: Color -> Tree -> Int -> Tree -> Tree
>	balanceL B (T R (T R a x b) y c) z d  =  (T R (T B a x b) y (T B c z d)) -- Left Left Case
>	balanceL B (T R a x (T R b y c)) z d = (T R (T B a x b) y (T B c z d)) -- Left Right case

>	balanceR :: Color -> Tree -> Int -> Tree -> Tree
>	balanceR B a q (T R b x (T R f r t)) = (T B (T R a q b) x (T R f r t)) -- Right Right case
>	balanceR B a x (T R (T R y z t) p r) = (T B (T R a x y) z (T R y p r)) -- Right Left case

