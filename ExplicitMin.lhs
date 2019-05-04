
> module ExplicitMin
>	  (Heap, empty, isEmpty, findMin,  deleteMin, merge) where

-- NOTE: without functors or type classes, we would manually swap in
-- different implementations of H by twiddling the following imports

> import qualified BHeaps as H

import qualified LHeaps as H 

> data Heap a = E | NE a (H.Heap a) deriving (Show)   -- the Int is the minimum element

> class EMHeap h where
>   empty     :: Ord a =>  h a
>   isEmpty   :: Ord a =>  h a -> Bool
>   findMin   :: Ord a => h a -> Maybe a
>   insert    :: Ord a => a  -> h a -> h a
>   deleteMin :: Ord a => h a  -> Maybe (h a)
>   merge     :: Ord a => h a  -> h a  -> h a


> empty' = E


> isEmpty' h = case h of
>             E    -> True
>             h    -> True


Insert new elem in then you need to find new min with H.findMin and and replace explicitmin with that


> insert' x E          = NE x singleton
>                       where singleton = H.insert x H.empty
> insert' x (NE min h) = NE newMin newIHeap
>                       where
>                            newIHeap = H.insert x h
>                            Just newMin   = H.findMin newIHeap

merge the two heaps. then whatever min is smaller among the two make that the new min. Finding the min of the merged heaps is O(1) 
even with Bheaps



> merge' E h@(NE _ heap1) = NE min mergedHeaps 
>                          where mergedHeaps = H.merge H.empty heap1 
>                                Just min = findMin h

> merge' h@(NE _ heap1) E = NE min mergedHeaps 
>                          where mergedHeaps = H.merge H.empty heap1 
>                                Just min = findMin h

> merge' h1@(NE _ heap1) h2@(NE _ heap2) = if min1 <= min2 then NE min1 mergedHeaps else NE min2 mergedHeaps  
>                                          where mergedHeaps = H.merge heap1 heap2
>                                                Just min1 = findMin h1
>                                                Just min2 = findMin h2 



> findMin' E          = Nothing
> findMin' (NE min _) = Just min


Delete the min then return a new heap with the new min of the internal heap

If Internalheap is empty after deleting the min then return the empty heap


> deleteMin' E = Nothing
> deleteMin' h@(NE min heap) = if newMin == Nothing then Just E else Just (NE newMin' newHeap) 
>                              where 
>                              Just newHeap = H.deleteMin heap
>                              newMin  = H.findMin newHeap 
>                              Just newMin' = H.findMin newHeap

> instance EMHeap Heap where
>     empty = empty'
>     isEmpty = isEmpty'
>     merge   = merge'
>     insert = insert'
>     findMin = findMin'
>     deleteMin = deleteMin' 