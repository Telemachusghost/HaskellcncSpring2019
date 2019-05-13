>	module RBTrees3 where

>	import RedBlackTrees 
>	import RBTrees2 (Balance, balanceLL, balanceLR, balanceRL, balanceRR)

>	insert :: Tree -> Int -> Tree
>	insert t x = T B l y r
>		where
>			((T _ l y r),_) = ins x t

>	ins :: Int -> Tree -> (Tree, (Balance, Balance))
>	ins x E = (T R E x E, (balanceLL,balanceLR))
>	ins x u@(T c l y r) 
>				| x == y	= (u,(balanceLL,balanceLR))
>				| x < y		= (bL c treeL y r,(balanceLL,balanceLR))  
>     			| otherwise 	= (bR c l y treeR,(balanceRL,balanceRR))
>	              where (treeL,(bL,bR)) = ins x l
>	                    (treeR,(bL2,bR2)) = ins x r


*RBTrees3>  buildRandom (RBTrees2.insert ) 5
T B (T B E 2031 (T R E 4316 E)) 5638 (T B E 8157 (T R E 9868 E))

*RBTrees3>  buildRandom (RBTrees2.insert ) 10
T B (T R (T B (T R E 1564 E) 2031 E) 2961 (T B E 4316 (T R E 5420 E))) 5638 (T R (T B E 5728 E) 8157 (T B E 8864 (T R E 9868 E)))