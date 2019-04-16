>  module BSTTree3
>	(BSTTree3, empty, isEmpty, insert, find, pre, post, inorder,
>	findMin, findMax, lchild, rchild, delete) where
>	import Tree

>	data BSTTree3 a = Nil | Leaf a | Branch a (BSTTree3 a) (BSTTree3 a)
>		deriving (Show)

>	instance Tree BSTTree3 where
>		empty = Nil
>		isEmpty t = case t of
>					Nil -> True
>					_ 	  -> False
>		insert x t = case t of 
>						Nil -> Leaf x 
>						Leaf a -> if x < a then Branch a (Leaf x) empty else if x > a then Branch a empty (Leaf x) else Leaf a
>						t@(Branch root  l r)     -> if x < root then  (Branch root (insert x l) r)  else if x > root then Branch root l (insert x r) else t 

>		find x t = case t of 
>					Nil -> False
>					Leaf a -> if x == a then True else False
>					(Branch root l r) -> if x == root then True else if x < root then find x l else find x r 
>		pre t = case t of 
>				Nil -> []
>				Leaf a -> [a]
>				(Branch x l r) -> [x] ++ pre l ++ pre r
>		post t = case t of
>				 Nil -> []
>				 Leaf a -> [a]
>				 (Branch x l r) -> post r ++ post l ++ [x]
>		inorder t = case t of
>					Nil -> []
>					Leaf a -> [a]
>					(Branch x l r) -> inorder l ++ [x] ++ inorder r
>		findMin t = case t of
>					Leaf a                -> a
>					(Branch root Nil _)   -> root
>					(Branch _ l _) 	      -> findMin l    
>		findMax t = case t of
>					Leaf a              -> a
>					(Branch root _ Nil) -> root
>					(Branch _ r _)       -> findMax r

Note the following are not in the tree interface, but I can still
implement them and export them. A user is free to use them BUT
if they want to use a "portable" tree that can be used anywhere
our typeclass is used, then these would be non-standard.
You can change that by putting them in to tree type class.

>	lchild Nil = empty
>	lchild (Branch _ l _) = l 	
>	rchild Nil = empty
>	rchild (Branch _ _ r) = r

>	replaceLChild l (Branch x _ r) = Branch x l r
>	replaceRChild r (Branch x l _) = Branch x l r

>	delete x t = case t of 
>					Nil -> Nil
>					Leaf a -> Nil
>					Branch root l r -> if x == root then Branch newRoot newLeft r else if x < root then Branch root (delete x l) r else Branch root l (delete x r)
>										where 
>										newRoot = findMax l
>										newLeft = delete newRoot l

