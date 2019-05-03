>	module LHeaps(Heap,makeT,merge,insert,fromList,findMin) where

{-- Copied from LeftistHeaps.lhs from class. DO NOT MODIFY. -------------}

>	type Rank = Int

>	data Heap a = E | T Rank a (Heap a) (Heap a)
>		deriving (Show)

>	rank :: Heap a -> Rank
>	rank E = 0
>	rank (T r _ _ _) = r

>	makeT :: a -> Heap a -> Heap a -> Heap a
>	makeT x h1 h2 = T (1 + rank r) x l r
>		where
>			(l,r) = if rank h1 >= rank h2 then (h1,h2)
>					else (h2,h1)

>	merge :: Ord a => Heap a -> Heap a -> Heap a
>	merge h1 h2 = case (h1, h2) of
>	  (_, E) -> h1
>	  (E, _) -> h2 
>	  (T _ x1 l1 r1, T _ x2 l2 r2) ->
>		if (x1 <= x2) then makeT x1 l1 (merge r1 h2)
>			else makeT x2 l2 (merge h1 r2)


{------------------------------------------------------------------------}



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


>	insert' x E = T 1 x E E 
>	insert' x (T rank p l r) = if x <= p then (T rank x newLeft r) else (T rank p newLeft' r) 
>	                           where newLeft = insert' p l 
>	                                 newLeft' = insert' x l	

So I think this would be n time still because you need to merge
every set of pairs so you are still running in O(n) time
 

creates a list of heaps that has log(n) space

>	mergePairs :: Ord a => [Heap a] -> [Heap a]
>	mergePairs  []        = []
>	mergePairs (h:[])    = [h]
>	mergePairs (h:h2:hs) = pair1:rest 
>                          where rest = mergePairs hs
>                                pair1 = merge h h2 



>	makePass :: Ord a => [Heap a] -> [Heap a] 
>	makePass []       = []
>	makePass (x:[])   = [x]
>	makePass list     = makePass $ mergePairs list 


>	fromList :: Ord a => [a] -> Heap a
>	fromList list = head . makePass $ fromListH list


>	fromListH :: Ord a => [a] -> [Heap a]
>	fromListH [] = []
>	fromListH (x:[])    = [(T 1 x E E)]
>	fromListH (x:x2:xs) =  merge1:(fromListH xs)
>                        where merge1 = merge (T 1 x E E) (T 1 x2 E E)  

>	findMin :: Heap a -> Maybe a
>	findMin h = case h of
>               E -> Nothing
>               (T _ x _ _ ) -> Just x


3.3

makepass calls merge on each list of merged pairs that mergepairs makes so in a series like this
n/2 + n/4 + n/8 ... 1 this is O(n) 

S(n,m) =  n/2 +  O(log(m))
S(2,2) =  1              	

T(n,m) = T(n/2,m) + n/2  + O(log(m)) -- T is ran n/2 times n/2 + log(m) are added because of mergepairs 
T(1,m) = 1

Analyzing S(n, m) O(logm) = x and O(n) = n

There are n/2 merges for each pass

T(n,m) =  T(n/2, m) + n/2 +  x
       =  T(n/4, m) + n/2 + n/4 + x + x
	   =  T(n/8, m) + n/2 + n/4 + n/8 + x + x + x
	   =  T(n/2^k, m) + k * x + n  -- The series n/2 + n/4 + n/8 .. + 1 is O(n)


n/2^k = 1
n = 2^(k)
log(n) = k 

T(1,m) = T(n/2^k, m) + log(m) + n
       = T(1, 1) + log(n)*log(m) + n
	   = 1 + log(n) * log(m) + n
	   = O(n)
 T(1,m) = 1 


This would make the worst case performance of this algorithm n
 

Extra credit implement findMin, deleteMin.


I already implemented findmin above

>	deleteMin (T _ _ E E) = E
>	deleteMin (T _ _ a b) = merge a b

>	empty = E

>	heapSort E = []
 
>	heapSort h = let 
>                    newH = deleteMin h 
>                    (Just min)  = findMin h
>                in (min):(heapSort newH) 

>	t1 = insert 5 $ insert 20 $ insert 9 $ insert (-5) $ empty


T 2 (-5) (T 1 9 E E) (T 1 5 (T 1 20 E E) E)
*LHeaps> deleteMin t1
T 2 5 (T 1 20 E E) (T 1 9 E E)

*LHeaps> heapSort t1
[-5,5,9,20]

*LHeaps> findMin t1
Just (-5)

>	t2 = insert "d" $ insert "e" $ insert "r" $ insert "i" $ insert "c" $ insert "k" $ empty


*LHeaps> t2
T 2 "c" (T 1 "k" E E) (T 1 "d" (T 1 "e" (T 1 "i" (T 1 "r" E E) E) E) E)
*LHeaps> deleteMin t2
T 2 "d" (T 1 "e" (T 1 "i" (T 1 "r" E E) E) E) (T 1 "k" E E)

*LHeaps> heapSort t2
["c","d","e","i","k","r"] (I have never heapSorted myself before)

*LHeaps> fromList "Derick is sherri's favorite student"
T 3 ' ' (T 3 ' ' (T 3 ' ' (T 3 ' ' (T 2 'd' (T 2 'i' (T 1 't' E E) (T 1 's' (T 1 't' E E) E)) (T 1 'u' E E)) (T 2 'e' (T 1 'f' E E) (T 1 'i' (T 1 's' E E) E))) (T 2 '\'' (T 2 'a' (T 1 'v' E E) (T 1 'o' (T 1 'r' E E) E)) (T 1 's' E E))) (T 2 'e' (T 2 'h' (T 1 's' E E) (T 1 'i' (T 1 'r' E E) E)) (T 1 'r' E E))) (T 2 'D' (T 2 'c' (T 1 'k' E E) (T 1 'i' (T 1 'r' E E) E)) (T 1 'e' (T 2 'e' (T 1 'n' E E) (T 1 't' E E)) E))

heapSort $ fromList "Derick is sherri's favorite student"
"    'Dacdeeeefhiiiiknorrrrsssstttuv"


