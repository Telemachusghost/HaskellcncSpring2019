>	module RBTrees1 where

>	import RedBlackTrees 

See if Haskell has something comparable to Debug in ELM
import Debug

>	insert :: Tree -> Int -> Tree
>	insert t x = T B l y r
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
>	balanceL a b c d = (T a b c d)

>	balanceR :: Color -> Tree -> Int -> Tree -> Tree
>	balanceR B a q (T R b x (T R f r t)) = (T B (T R a q b) x (T R f r t)) -- Right Right case
>	balanceR B a x (T R (T R y z t) p r) = (T B (T R a x y) z (T R y p r)) -- Right Left case
>	balanceR a b c d = (T a b c d)

Testing

*RBTrees1>  buildRandom (RBTrees1.insert ) 5
T B (T B E 2031 (T R E 4316 E)) 5638 (T B E 8157 (T R E 9868 E))

*RBTrees1>  buildRandom (RBTrees1.insert ) 10
T B (T R (T B E 1564 E) 2031 (T B (T R E 2961 E) 4316 (T R E 5420 E))) 5638 (T R (T B E 5728 E) 8157 (T B E 8864 (T R E 9868 E)))
