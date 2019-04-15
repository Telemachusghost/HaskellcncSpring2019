>	module LHeaps(Heap,makeT,merge,insert,fromList,findMin) where

{-- Copied from LeftistHeaps.lhs from class. DO NOT MODIFY. -------------}

>	type Rank = Int

>	data Heap = E | T Rank Int Heap Heap
>		deriving (Show)

>	rank :: Heap -> Rank
>	rank E = 0
>	rank (T r _ _ _) = r

>	makeT :: Int -> Heap -> Heap -> Heap
>	makeT x h1 h2 = T (1 + rank r) x l r
>		where
>			(l,r) = if rank h1 >= rank h2 then (h1,h2)
>					else (h2,h1)

>	merge :: Heap -> Heap -> Heap
>	merge h1 h2 = case (h1, h2) of
>	  (_, E) -> h1
>	  (E, _) -> h2 
>	  (T _ x1 l1 r1, T _ x2 l2 r2) ->
>		if (x1 <= x2) then makeT x1 l1 (merge r1 h2)
>			else makeT x2 l2 (merge h1 r2)


{------------------------------------------------------------------------}



>	insert :: Int -> Heap -> Heap
>	insert x h = merge (T 1 x E E) h

3.2

attempt at insert with upper and lower bound of n

So the property of leftist trees is that the left tree needs to be at least as large as its right sibling.
So to make an insert that has an upwards boud of n simply just only insert on the left subtree. This means that all inserted
nodes will be on the left side which makes it so that insert can take upto n calls because all inserted nodes are on the left side.

On a tree where insert is used repeatedly the lower bound is n because every node is inserted in the left subtree which basically
lays the tree out like a list. 

When timed using ghci with insert applied to an empty heap 1000 times
insert: 1.88 sec
insert': 3.28 sec 

which fits because log_2(3.28) = 1.71

>	insert' :: Int -> Heap -> Heap
>	insert' x E = T 1 x E E 
>	insert' x (T rank p l r) = if x <= p then (T rank x newLeft r) else (T rank p newLeft' r) 
>	                           where newLeft = insert' p l 
>	                                 newLeft' = insert' x l	

So I think this would be n time still because you need to merge
every set of pairs so you are still running in O(n) time
 

creates a list of heaps that has log(n) space

>	mergePairs :: [Heap] -> [Heap]
>	mergePairs  []        = []
>	mergePairs (h:[])    = [h]
>	mergePairs (h:h2:hs) = pair1:rest 
>                          where rest = mergePairs hs
>                                pair1 = merge h h2 



>	makePass :: [Heap] -> [Heap] 
>	makePass []       = []
>	makePass (x:[])   = [x]
>	makePass list     = makePass $ mergePairs list 


>	fromList :: [Int] -> Heap
>	fromList list = head . makePass $ fromListH list


>	fromListH :: [Int] -> [Heap]
>	fromListH [] = []
>	fromListH (x:[])    = [(T 1 x E E)]
>	fromListH (x:x2:xs) =  merge1:(fromListH xs)
>                        where merge1 = merge (T 1 x E E) (T 1 x2 E E)  

>	findMin :: Heap -> Maybe Int
>	findMin h = case h of
>               E -> Nothing
>               (T _ x _ _ ) -> Just x


3.3

makepass calls merge on each list of merged pairs that mergepairs makes so in a series like this
n/2 + n/4 + n/8 ... 1 this is O(n) 

S(n,m) =  n/2 +  O(log(m))
S(2,2) =  n/2 + O(1)	-- Since the base case is so different I did not really know what to do there.

T(n,m) = T(n/2,m) + O(log(m)) -- T runs in n/2 half time because of merge pair running
T(1,m) = 1

Analyzing S(n, m) O(logm) = x

T(n,m) =  T(n/2, m) +  x
       =  T(n/4, m) +  x + x
	   =  T(n/8, m) +  x + x + x
	   =  T(n/2^k, m) + k * x  

We can see from this that makepass is growing at a constant rate because each time mergepass is ran we are having to do another merge operation
this makes it O(n) 

n/2^k = 1
n = 2^(k)
log(n) = k 

T(n,m) = T(n/2^k, m) + O(log(m))
       = T(1, 1) + log(n)*log(m)
	   = 1 + log(n) * log(m) 

 T(1,1) = 1 + 0





