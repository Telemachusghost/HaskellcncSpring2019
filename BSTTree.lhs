>  module BSTTree
>	(BSTTree, empty, isEmpty, insert, find, pre, post, inorder,
>	findMin, findMax, lchild, rchild, delete) where
>	import Tree

>	data BSTTree a = Empty | Node a (BSTTree a) (BSTTree a)
>		deriving (Show)

>	instance Tree BSTTree where
>		empty = Empty
>		isEmpty t = case t of
>					empty -> True
>					_ 	  -> False
>		insert x t = case t of 
>					 Empty -> Node x empty empty
>					 t@(Node root l r)     -> if x < root then (Node root (insert x l) r)  else if x > root then (Node root l (insert x r)) else t 

>		find x t = case t of 
>					Empty -> False
>					Node root l r -> if x == root then True else if x < root then find x l else find x r 
>		pre t = case t of 
>				Empty -> []
>				Node x l r -> [x] ++ pre l ++ pre r
>		post t = case t of
>				 Empty -> []
>				 Node x l r -> post r ++ post l ++ [x]
>		inorder t = case t of
>					Empty -> []
>					Node x l r -> inorder l ++ [x] ++ inorder r
>		findMin t = case t of
>					Node root Empty _ -> root
>					Node _ l _ 	      -> findMin l    
>		findMax t = case t of
>					Node root _ Empty -> root
>					Node _ _ r -> findMax r

Note the following are not in the tree interface, but I can still
implement them and export them. A user is free to use them BUT
if they want to use a "portable" tree that can be used anywhere
our typeclass is used, then these would be non-standard.
You can change that by putting them in to tree type class.

>	lchild Empty = empty
>	lchild (Node _ l _) = l 	
>	rchild Empty = empty
>	rchild (Node _ _ r) = r

>	replaceLChild l (Node x _ r) = Node x l r
>	replaceRChild r (Node x l _) = Node x l r 

>	delete x t = case t of 
>				 Empty -> Empty
>				 Node root Empty Empty -> Empty
>				 Node root l r -> if x == root then Node newRoot newLeft r else if x < root then Node root (delete x l) r else Node root l (delete x r)
>				 				  where newRoot = findMax l
>				       					newLeft = delete newRoot l


