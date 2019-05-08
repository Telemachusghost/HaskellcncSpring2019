>	module RBTrees2 where

>	import RedBlackTrees 
>	import Prelude hiding (Left, Right)

>	insert :: Int -> Tree -> Tree
>	insert x t = T B l y r
>		where
>			((T _ l y r),_) = ins x t []

			ins :: Int -> Tree -> Tree
			ins x E = (T R E x E, [])
			ins x u@(T c l y r)
				| x == y	= u
				| x < y		= balance c (ins x l) y r
     			| otherwise 	= balance c l y (ins x r)

>	data Dir = Left | Right
>		deriving (Show, Eq)

>	ins :: Int -> Tree -> [Dir] -> (Tree, [Dir])
>	ins x E dir = (T R E x E, dir)
>	ins x u@(T c l y r) dir
>				| x == y	= (u,dir)
>				| x < y		= (balanceL c leftTree y r,(Left:dirL))  
>     			| otherwise 	= (balanceR c l y rightTree,(Right:dirR))
>	            where (leftTree,dirL) = ins x l dir
>	                  (rightTree,dirR) = ins x r dir
>	                  balanceL = chooseBalance (Left:dirL)
>	                  balanceR = chooseBalance (Right:dirR)
>	

>	type Balance = Color -> Tree -> Int -> Tree -> Tree

>	chooseBalance :: [Dir] -> Balance
>	chooseBalance dir = case dir of
>	                    (Left:Right:_)   -> balanceLR
>	                    (Left:Left:_)  -> balanceLL
>	                    (Right:Left:_)  -> balanceRL
>	                    (Right:Right:_) -> balanceRR
>	                    t               -> balanceID

>	balanceLL :: Balance
>	balanceLL B (T R (T R a x b) y c) z d  =  (T R (T B a x b) y (T B c z d))
>	balanceLL a b c d = (T a b c d)

>	balanceLR :: Balance
>	balanceLR B (T R a x (T R b y c)) z d = (T R (T B a x b) y (T B c z d))
>	balanceLR a b c d = (T a b c d)

>	balanceRL :: Balance
>	balanceRL B a x (T R (T R y z t) p r) = (T B (T R a x y) z (T R y p r))
>	balanceRL a b c d = (T a b c d)

>	balanceRR :: Balance
>	balanceRR B a q (T R b x (T R f r t)) = (T B (T R a q b) x (T R f r t))
>	balanceRR a b c d = (T a b c d)

>	balanceID a b c d = (T a b c d)


*RBTrees2> RBTrees2.insert (-50) $ RBTrees2.insert 25 $ RBTrees2.insert (1) $  RBTrees2.insert 90 $  RBTrees2.insert (-4) $ RBTrees2.insert 45 $ empty
T B (T B (T R E (-50) E) (-4) E) 1 (T B (T R E 25 E) 45 (T R E 90 E))
(0.01 secs, 136,512 bytes)

