
>	module BHeaps
>	  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge) where


>	data Tree a = Node a ([Tree a])
>		deriving (Eq, Show)
>	type Rank = Int
>	type InternalHeap a = [(Rank, Tree a)]
>	data Heap a = WrapHeap (InternalHeap a)
>		deriving (Eq, Show)

{-- Internal Helpers ----------------------------------------------------}

-- TODO

>	rootLess :: Ord a => (Rank, Tree a) -> (Rank, Tree a) -> Bool
>	rootLess (_,Node x _) (_,Node x2 _) | x < x2 = True
>                                       | True = False
 

>	link :: Ord a => (Rank, Tree a) -> (Rank, Tree a) -> (Rank, Tree a)
>	link h@(r, t@(Node x l)) h2@(_, t2@(Node x2 l2)) = if hLess then  ((r+1),Node x (t2:l))
>                                                               else  ((r+1),Node x2 (t:l2))
>                                                      where hLess = rootLess h h2

>	rank :: (Rank, Tree a) -> Rank
>	rank (r, _) = r

>	insertTree :: Ord a => (Rank, Tree a) -> InternalHeap a -> InternalHeap a
>	insertTree t [] = [t]
>	insertTree t ts@(t2:ts2) = if (rank t) < (rank t2) then (t:ts)
>                                                      else insertTree (link t t2) ts2 

>	root (Node x _ ) = x

>	getIHeap (WrapHeap h) = h



>	removeMinTree (WrapHeap [t])    = (t, [])
>	removeMinTree (WrapHeap (t:ts)) =  let (t', ts') = removeMinTree (WrapHeap ts)
>                                      in if rootLess t t' then (t,ts) else (t', t:ts')


This Function is used for deleteMin since we dont have rank information for each tree in this interpretation we need to add rank info in r-1 steps
then call a merge on the reversed list. 

This uses reverse which I assume is not a problem since it is what Okasaki recommended

>	mergeChildrenh _ []      = []
>	mergeChildrenh  r (t:ts) = ((r-1,t):(mergeChildrenh newR ts))
>	                           where newR = r-1
>	mergeChildren _ [] h = h 
>	mergeChildren r ts heap = merge (WrapHeap $ reverse (mergeChildrenh r ts)) heap  

{-- External Interface --------------------------------------------------}

>	empty :: Heap a
>	empty = WrapHeap []

>	isEmpty :: Ord a => Heap a -> Bool
>	isEmpty h = h == empty

>	insert :: Ord a =>  a -> Heap a -> Heap a
>	insert x (WrapHeap ts) = WrapHeap (insertTree (0, Node x []) ts ) 


>	merge :: Ord a => Heap a -> Heap a -> Heap a
>	merge h1@(WrapHeap t) h2@(WrapHeap []) = h1
>	merge h1@(WrapHeap []) h2@(WrapHeap t)  = h2
>	merge h1@(WrapHeap ts1'@(t1:ts1)) h2@(WrapHeap ts2'@(t2:ts2)) = if (rank t1) < rank t2 then t1Less
>                                                                                         else if (rank t2) < (rank t1) then t1More             
>                                                                                         else heapsEqual
>                                                                   where t1Less     = (WrapHeap (t1:(getIHeap $ merge  (WrapHeap ts1) (WrapHeap ts2'))))
>                                                                         t1More     = WrapHeap (t2:(getIHeap $ merge  (WrapHeap ts1') (WrapHeap ts2)))
>                                                                         heapsEqual = WrapHeap (insertTree (link t1 t2) (getIHeap $ merge (WrapHeap ts1) (WrapHeap ts2))) 

>	findMin :: Ord a => Heap a -> Maybe a
>	findMin h =  case isEmpty h of
>                True -> Nothing
>                False -> let ((_,t), _) = removeMinTree h in Just $ root t


>	deleteMin :: Ord a => Heap a -> Maybe (Heap a)
>	deleteMin h = case isEmpty h of
>                 True  -> Just (WrapHeap [])
>                 False -> Just (mergeChildren r ts3  remainingHeap)
>                          where 
>                                remainingHeap = WrapHeap ts2
>                                ((r,Node _ ts3), ts2) = removeMinTree h


>	heapSort (WrapHeap []) = []
>	heapSort h = let Just min = findMin h
>	                 Just newH = deleteMin h
>	             in min:(heapSort newH) 

>	t1 = insert "b" $ insert "u" $ insert "u" $ insert "n" empty 
> 	t2 = insert "f" $ insert "a" $ insert "b" $ insert "b" empty 

*BHeaps> heapSort t1
["b","n","u","u"]

*BHeaps> deleteMin t1
Just (WrapHeap [(0,Node "u" []),(1,Node "n" [Node "u" []])])

*BHeaps> deleteMin (merge t1 t2)
Just (WrapHeap [(0,Node "f" []),(1,Node "b" [Node "b" []]),(2,Node "b" [Node "n" [Node "u" []],Node "u" []])])

