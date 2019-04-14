>	module LHeaps(Heap,makeT,merge,insert,fromList) where

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

So I think this would be n time still because you need to merge
every set of pairs so you are still running in O(n) time
 


>	mergePairs :: [Heap] -> [Heap]
>	mergePairs  []        = []
>	mergePairs (h:[])    = [h]
>	mergePairs (h:h2:hs) = pair1:rest 
>                          where rest = mergePairs hs
>                                pair1 = merge h h2 

there are log(n) heaps now so we are doing 

>	makePass :: [Heap] -> [Heap] 
>	makePass []       = []
>	makePass (x:[])   = [x]
>	makePass list     = makePass $ mergePairs list 

So Im thinking the total expression for the function is
log**2 + log**3 + log**2

>	fromList :: [Int] -> Heap
>	fromList list = head . makePass $ fromListH list

log**2 because merge is log and we are doing this log times

>	fromListH :: [Int] -> [Heap]
>	fromListH [] = []
>	fromListH (x:[])    = [(T 1 x E E)]
>	fromListH (x:x2:xs) =  merge1:(fromListH xs)
>                        where merge1 = merge (T 1 x E E) (T 1 x2 E E)  


