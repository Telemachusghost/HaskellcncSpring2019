> import qualified BHeaps as B
> import qualified LHeaps as L



> class Heap h where
>   empty     :: Ord a =>  h a
>   isEmpty   :: Ord a =>  h a -> Bool
>   findMin   :: Ord a => h a -> Maybe a
>   insert    :: Ord a => a  -> h a -> h a
>   deleteMin :: Ord a => h a  -> Maybe (h a)
>   merge     :: Ord a => h a  -> h a  -> h a

> class EMHeap h  where
>   empty     :: (Ord a, Heap h) =>  h a
>   isEmpty   :: (Ord a, Heap h) =>  h a -> Bool
>   findMin   :: (Ord a, Heap h) => h a -> Maybe a
>   insert    :: (Ord a, Heap h) => a  -> h a -> h a
>   deleteMin :: (Ord a, Heap h) => h a  -> Maybe (h a)
>   merge     :: (Ord a, Heap h) => h a  -> h a  -> h a

> instance Heap (B.Heap) where
>     empty = B.empty
>     isEmpty = B.isEmpty
>     merge   = B.merge
>     insert = B.insert
>     findMin = B.findMin
>     deleteMin = B.deleteMin 

 instance Heap (L.Heap) where
     empty = L.empty
     isEmpty = L.isEmpty
     merge   = L.merge
     insert = L.insert
     findMin = L.findMin
     deleteMin = L.deleteMin 