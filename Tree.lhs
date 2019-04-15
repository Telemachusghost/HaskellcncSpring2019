>	module Tree (Tree(..)) where 
>	  class Tree t where
>		empty   :: Ord a => t a
>		isEmpty :: Ord a => t a -> Bool
>		insert  :: Ord a => a -> t a -> t a
>		find    :: Ord a => a -> t a -> Bool
>		pre	:: Ord a => t a -> [a]
>		post	:: Ord a => t a -> [a]
>		inorder	:: Ord a => t a -> [a]
>		findMin	:: Ord a => t a -> a
>		findMax	:: Ord a => t a -> a