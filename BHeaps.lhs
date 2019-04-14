
>	module BHeaps
>	  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge) where


>	data Tree = Node Int ([Tree])
>		deriving (Eq, Show)
>	type Rank = Int
>	type InternalHeap = [(Rank, Tree)]
>	data Heap = WrapHeap InternalHeap
>		deriving (Eq, Show)

{-- Internal Helpers ----------------------------------------------------}

-- TODO

>	rootLess :: (Rank, Tree) -> (Rank, Tree) -> Bool
>	rootLess (_,Node x _) (_,Node x2 _) | x < x2 = True
>                                       | True = False
 

>	link :: (Rank, Tree) -> (Rank, Tree) -> (Rank, Tree)
>	link h@(r, t@(Node x l)) h2@(_, t2@(Node x2 l2)) = if hLess then  ((r+1),Node x (t2:l))
>                                                               else  ((r+1),Node x2 (t:l2))
>                                                      where hLess = rootLess h h2

>	rank :: (Rank, Tree) -> Rank
>	rank (r, _) = r

>	insertTree :: (Rank, Tree) -> InternalHeap -> InternalHeap
>	insertTree t [] = [t]
>	insertTree t ts@(t2:ts2) = if (rank t) < (rank t2) then (t:ts)
>                                                      else insertTree (link t t2) ts2 

>	root (Node x _ ) = x

>	getIHeap (WrapHeap h) = h



>	removeMinTree (WrapHeap [t])    = (t, [])
>	removeMinTree (WrapHeap (t:ts)) =  let (t', ts') = removeMinTree (WrapHeap ts)
>                                      in if rootLess t t' then (t,ts) else (t', t:ts')

Because of the implemenation it causes issues with deletemin because rank is not associated with an actual tree but a heap
this is a function to take a tree and create an internalheap from its children

>	getIHeapFromChildren :: (Rank, Tree) -> InternalHeap
>	getIHeapFromChildren (r, (Node x ts) ) = treeFunctions <*> [topRank..0]
>                                            where treeFunctions =  map ( \t r -> (r, t)) ts  
>                                                  topRank       = r-1

{-- External Interface --------------------------------------------------}

>	empty :: Heap
>	empty = WrapHeap []

>	isEmpty :: Heap -> Bool
>	isEmpty h = h == empty

>	insert :: Int -> Heap -> Heap
>	insert x (WrapHeap ts) = WrapHeap (insertTree (0, Node x []) ts ) 


  
	data Tree = Node Int ([Tree])
		deriving (Eq, Show)
	type Rank = Int
	type InternalHeap = [(Rank, Tree)]
	data Heap = WrapHeap InternalHeap
		deriving (Eq, Show)



>	merge :: Heap -> Heap -> Heap
>	merge h1@(WrapHeap t) h2@(WrapHeap []) = h1
>	merge h1@(WrapHeap []) h2@(WrapHeap t)  = h2
>	merge h1@(WrapHeap ts1'@(t1:ts1)) h2@(WrapHeap ts2'@(t2:ts2)) = if (rank t1) < rank t2 then t1Less
>                                                                                         else if (rank t2) < (rank t1) then t1More             
>                                                                                         else heapsEqual
>                                                                   where t1Less     = (WrapHeap (t1:(getIHeap $ merge  (WrapHeap ts1) (WrapHeap ts2'))))
>                                                                         t1More     = WrapHeap (t2:(getIHeap $ merge  (WrapHeap ts1') (WrapHeap ts2)))
>                                                                         heapsEqual = WrapHeap (insertTree (link t1 t2) (getIHeap $ merge (WrapHeap ts1) (WrapHeap ts2))) 

>	findMin :: Heap -> Maybe Int
>	findMin h =  case isEmpty h of
>                True -> Nothing
>                False -> let ((_,t), _) = removeMinTree h in Just $ root t


>	deleteMin :: Heap -> Maybe Heap
>	deleteMin h = case isEmpty h of
>                 True  -> Nothing
>                 False -> Just (merge childrenHeap remainingHeap)
>                          where childrenHeap  = WrapHeap $ getIHeapFromChildren p
>                                remainingHeap = WrapHeap ts2
>                                (p, ts2) = removeMinTree h



