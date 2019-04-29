> module Heaps (Heap, empty, isEmpty, findMin, insert, deleteMin, merge) where


> import Data.Array


> class Heap h where
>   empty     :: Ord a => h a
>   isEmpty   :: Ord a => h a -> Bool
>   findMin   :: Ord a => h a -> Maybe a
>   insert    :: Ord a => a  -> h a -> h a
>   deleteMin :: Ord a => h a -> Maybe (h a)
>   merge     ::  Ord a => h a -> h a -> h a

> type LastUsed = Int
> data MyHeap a = Empty | H LastUsed  (Array Int a) deriving(Show)

> mkArray xs = array (1,n) $ zip (range (1, n)) xs
>              where n = length xs

> swap i j h = h // [(i, tmp2), (j, tmp1)]
>              where tmp1 = h!i
>                    tmp2 = h!j

> bubbleUp child h | child == 1 = h
>                  | otherwise = if h ! parIndex < h ! child then h
>                                else bubbleUp parIndex $ swap parIndex child h
>                                where parIndex = div child 2


> bubbleDown current (H lst heap) | current >= lst = heap
>	                            | lchild > lst	= heap

>	                            | rchild > lst	= if curVal < lval then heap
>			                                      else bubbleDown lchild $ H lst (swap current lchild heap)
>	                            | otherwise	= if newCur == current then heap
>			                                  else bubbleDown newCur $ H lst (swap current newCur heap)
>								where
>								lchild = 2*current
>								rchild = 2*current + 1
>								lval = heap!lchild
>								rval = heap!rchild
>								curVal = heap ! current
>								newCur = if curVal < lval && curVal < rval then current else if lval < rval then lchild else rchild

        



> buildHeap lst h     = H lst (buildHeap' lst (lst `div` 2) h)

> buildHeap' lst currentSz h
>		                | currentSz > 0 = buildHeap' lst (currentSz-1) $ bubbleDown currentSz (H lst h)
>			          | otherwise	= h



> heapSort Empty = []
> heapSort heap@(H lst h) = min : (heapSort $ rest)
>                           where Just rest = delMin
>                                 delMin = deleteMin heap
>                                 fMin = findMin heap
>                                 Just min = findMin heap 

This was my first attempt

> insert' x (H lst h) = if newLast <= end then  newHeap else error "Heap is full!"
>                       where
>                       (fst, end) = bounds h
>                       newLast = lst+1
>                       unused =  take (end - newLast) $ repeat x
>                       newHeap = (buildHeap newLast (h // [(newLast,x)]))

This was my second attempt after notes from Sherri

> insert'' x Empty     = (H 1 (mkArray [x]))
> insert'' x (H lst h) = if lst == lst2 then H (lst+1) (bubbleUp (lst+1) ((grow h lst) // [(lst+1,x)]))   else (H (lst+1) (bubbleUp (lst+1) (h // [(lst+1,x)]))) 
>                        where (fst,lst2) = bounds h

> grow h lst = mkArray ((elems h) ++ (take (lst*2) $ repeat (h ! 1)))



> instance Heap MyHeap where
>    empty = Empty

>    isEmpty heap = case heap of 
>                   Empty -> True
>                   otherwise -> False 

>    findMin Empty   = Nothing
>    findMin (H _ a) = Just (a ! 1)  
 
>    deleteMin Empty = Nothing
>    deleteMin (H 1 _)       = Just Empty
>    deleteMin (H last heap) = Just (H (last-1) (bubbleDown 1 $ H (last-1) (((swap fst last heap)) )))
>                              where (fst,_) = bounds heap

I wasnt sure if you how you wanted us to grow a merged heap unused space wise so I just added the used up space of both heaps and added that to the merged heap
This is inefficient, but having to keep track of used space like this in haskell seems clumsy

>    merge (H lst1 h) (H lst2 h2) = H newLast heap
>                                   where 
>                                         list1   = take lst1 $ elems h
>                                         list2   = take lst2 $ elems h2
>                                         unused  = take newLast $ repeat (h ! 0)
>                                         comList = list1 ++ list2 ++ unused 
>                                         tHeap   = buildHeap newLast (mkArray comList)
>                                         heap    =  mkArray ( (heapSort tHeap) ++ unused ) 
>                                         newLast = lst1 + lst2
>                                         

>    insert x h = insert'' x h

testing

> maxInt = maxBound :: Int

> t1 = buildHeap 5 (mkArray [2,6,-1,20,3,maxInt,maxInt,maxInt]) :: MyHeap Int-- This wont organize beyond index 5

*Heaps> t1
H 5 (array (1,8) [(1,-1),(2,3),(3,2),(4,20),(5,6),(6,-500),(7,43),(8,42)])
*Heaps> heapSort t1
[-1,2,3,6,20]



> t2 = buildHeap 8 (mkArray [2,6,-1,20,3,-500,43,42]) :: MyHeap Int -- Sorts full heap

*Heaps> heapSort t2
[-500,-1,2,3,6,20,42,43]

> t3 = buildHeap 1 (mkArray ([2] ++ (take 7 $ repeat maxInt))) :: MyHeap Int -- Min test

*Heaps> heapSort t3
[2]

merging/insertion tests

> t4 = merge t1 t3

*Heaps> t4
H 6 (array (1,12) [(1,-1),(2,2),(3,2),(4,3),(5,6),(6,20),(7,9223372036854775807),(8,9223372036854775807),(9,9223372036854775807),(10,9223372036854775807),(11,9223372036854775807),(12,9223372036854775807)])

*Heaps> insert 42 t4
H 7 (array (1,12) [(1,-1),(2,2),(3,2),(4,3),(5,6),(6,20),(7,42),(8,9223372036854775807),(9,9223372036854775807),(10,9223372036854775807),(11,9223372036854775807),(12,9223372036854775807)])

Heap is Full!

*Heaps> let r = buildHeap 1 $ mkArray [2,maxInt]
*Heaps> insert 4 . insert 2 $ r
*** Exception: Heap is full!
CallStack (from HasCallStack):
  error, called at Heaps.lhs:66:62 in main:Heaps
*Heaps>

  