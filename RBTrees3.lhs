>	module RBTrees3 where

>	import RedBlackTrees 
>	import RBTrees2 (Balance, balanceLL, balanceLR, balanceRL, balanceRR)

>	insert :: Int -> Tree -> Tree
>	insert x t = T B l y r
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


*RBTrees3> RBTrees3.insert (100) $ RBTrees3.insert (-50) $ RBTrees3.insert (50) $ RBTrees3.insert (-20) $ RBTrees3.insert 20 $ RBTrees3.insert 5 $ empty
T B (T B (T R E (-50) E) (-20) E) 5 (T B E 20 (T R E 50 (T R E 100 E)))