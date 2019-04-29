>  module BSTTree2
>	(BSTTree2, empty, isEmpty, insert, find, pre, post, inorder,
>	findMin, findMax, lchild, rchild, delete) where
>	import Tree

>	data BSTTree2 a = Nil | N (a, (BSTTree2 a), (BSTTree2 a))
>		deriving (Show)

>	instance Tree BSTTree2 where
>		empty = Nil
>		isEmpty t = case t of
>					Nil -> True
>					_ 	  -> False
>		insert x t = case t of 
>                    Nil -> N (x, Nil, Nil)
>                    t@(N (root, l, r))     -> if x < root then N ( root, (insert x l), r)  else if x > root then N (root, l, (insert x r)) else t 

>		find x t = case t of 
>					Nil -> False
>					(N (root, l, r)) -> if x == root then True else if x < root then find x l else find x r 
>		pre t = case t of 
>				Nil -> []
>				(N (x, l, r)) -> [x] ++ pre l ++ pre r
>		post t = case t of
>				 Nil -> []
>				 (N (x, l, r)) -> post r ++ post l ++ [x]
>		inorder t = case t of
>					Nil -> []
>					(N (x, l, r)) -> inorder l ++ [x] ++ inorder r
>		findMin t = case t of
>					(N (root, Nil, _)) -> root
>					(N (_, l, _)) 	      -> findMin l    
>		findMax t = case t of
>					(N (root, _, Nil)) -> root
>					(N (_, r, _)) -> findMax r

Note the following are not in the tree interface, but I can still
implement them and export them. A user is free to use them BUT
if they want to use a "portable" tree that can be used anywhere
our typeclass is used, then these would be non-standard.
You can change that by putting them in to tree type class.

>	lchild Nil = empty
>	lchild (N (_, l, _)) = l 	
>	rchild Nil = empty
>	rchild (N (_, _, r)) = r

>	replaceLChild l (N (x, _, r)) = N (x, l, r)
>	replaceRChild r (N (x, l, _)) = N (x, l, r) 

>	delete x t = case t of 
>				 Nil -> Nil
>				 N (root, Nil, Nil) -> Nil
>				 N (root, l, r) -> if x == root then N (newRoot, newLeft, r) else if x < root then N (root, (delete x l), r) else N (root, l, (delete x r))
>									where 
>									newRoot = findMax l
>									newLeft = delete newRoot l

