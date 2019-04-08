> module Heaps (Heap, empty, isEmpty, findMin, insert, deleteMin, merge) where

I tried to follow along with your code, but I feel like I am struggling with using haskell to implement a lot of things
because of typing I cant use infinity because of this typing error involving fractional integer

> import Data.Array

> data Heap a = Empty | H (Array Int a) deriving(Show)

> mkArray xs = array (1,n) $ zip (range (1, n)) xs
>              where n = length xs

> swap i j h = h // [(i, tmp2), (j, tmp1)]
>              where tmp1 = h!i
>                    tmp2 = h!j

> bubbleUp child h | child <= 1 = h
>                  | otherwise = if h ! parIndex < h ! child then h
>                                else bubbleUp parIndex $ swap parIndex child h
>                                where parIndex = div child 2


> bubbleDown current (H heap)	| current >= lst = heap
>	                            | lchild > lst	= heap

>	                            | rchild > lst	= if curVal < lval then heap
>			                                      else bubbleDown lchild $ H (swap current lchild heap)
>	                            | otherwise	= if newCur == current then heap
>			                                  else bubbleDown newCur $ H (swap current newCur heap)
>		                        where
>			                    (fst,lst) = bounds heap
>			                    lchild = 2*current
>			                    rchild = 2*current + 1
>			                    lval = heap!lchild
>			                    rval = heap!rchild
>			                    curVal = heap ! current
>			                    newCur = if curVal < lval && curVal < rval then current else if lval < rval then lchild else rchild


> empty :: Ord a => Heap a
> empty = Empty

> isEmpty :: Ord a => Heap a -> Bool
> isEmpty heap = case heap of 
>                Empty -> True
>                otherwise -> False 

> findMin :: Ord a => Heap a -> a
> findMin (H a) = a ! 1           

Returns index of last element used

> findLast arr acc l | acc > l = 0
>                    | arr ! acc == 10^6 = acc 
>                    | otherwise = findLast arr (acc+1) l

> deleteMin :: (Ord a, Num a) => Heap a -> Heap a
> deleteMin (H heap) = H (bubbleDown 1 $ H (((swap fst lst heap)) // [(lst, 10^6)]))
>                      where (fst,lst) = bounds heap

> buildHeap h     = H (buildHeap' (lst `div` 2) h)
>	            	where
>		        	(fst, lst) = bounds h

> buildHeap' currentSz h
>		                | currentSz > 0 = buildHeap' (currentSz-1) $ bubbleDown currentSz (H h)
>			            | otherwise	= h


> heapSort :: (Ord a, Num a) => Heap a -> [a]
> heapSort heap@(H h) 

>     	       | h ! 1 == 10^6   = []
>	           | otherwise	   = findMin heap : (heapSort $ (deleteMin heap))


So I was not really able to get infinity to work correctly so I will just have 10^6 indicate the last used element
Would the heap you be inserting into only have room for one more node?

> insert :: Integer -> Heap Integer -> Heap Integer
> insert _ _ = Empty

 insert elem heap@(H h) = if lastElem <= l then fullSortedNewHeap else H (sortedArray // [(lastElem+1, 10^6)])  
                          where lastElem = findLast h f l
                                newHeap = h // [(lastElem, elem)]
                                sortedArray =  (mkArray $ bubbleDown lastElem (H newHeap))
                                fullSortedNewHeap = H sortedArray
                                nextEmpty = lastElem+1
                                (f,l) = bounds h

So con both the lists O(n) time
then heapsort the result

> merge :: (Num a, Ord a) => Heap a -> Heap a -> Heap a
> merge (H h) (H h2) = H (mkArray $ heapSort heap)
>                      where list1 = elems h
>                            list2 = elems h2
>                            comList = list1 ++ list2 
>                            heap    = buildHeap (mkArray comList)

