Derick Falk
Haskell MidTerm

Question 0: Use the bubbleUp algorithm to insert 1 into the binary heap (draw the initial tree and the final tree):

BinaryHeap> let t = (empty::BinaryHeap Int)
*BinaryHeap> insert 3 $ insert 5 $ insert 2 $ insert 7 t
H (array (1,10) [(1,2),(2,3),(3,5),(4,7),(5,5),(6,5),(7,5),(8,5),(9,5),(10,5)],4)
*BinaryHeap> 

1 is inserted into last position of the list and then bubbleup is called

Inital tree
 
           1
          / \
         3   5
        /    
       7  


After inserting 1 and then the bubbleup

           1
          / \
         1   5
        / \   
       7   3


Question 1: In the implementation of LHeaps.lhs (leftist heaps -- hw4)
answer the following questions:


a) What is the role of Rank?
b) How do the makePass and mergePairs work to create one Heap
Show how it works on the following list:

	heaps = [insert 1 E, insert 2 E, insert 3 E, insert 4 E]

c) How would you implement this using foldr or foldl


d) In the following sequence what is the result (do not use
the Haskell interpreter to get this result):

	t1 = insert 1 $ insert 5 $ insert 3 $ insert 8 E
	t2 = insert 8 $ insert 6 $ insert 4 $ insert 2 E

e) findMin and deleteMin were not implemented in LHeaps. Implement
them. Assume you can import LHeaps.

****************Answers************

a) Rank keeps track of the Leftist heap property. 
   The leftist heap property is the property that the left subtree is always greater than or equal to the right. This allows for faster merging.

b) mergepairs merges every two heaps in a list or one in the case of a singleton list.
   makepass calls mergepairs until the list is a singleton list.
   
   With the example heaps denoted by [h1, h2, h3, h4]
   
   makepass is first called which calls mergepairs

   mergepairs merges every two heaps in the list returning [h1+h2,h3+h4]

   makepass sees that what it has is still not a singleton list so it calls mergepairs again

   mergepairs merges h1+h2 and h3+h4 leading to a singleton list of [h1+h2+h3+h4]

   makepass sees it has a singleton list so it returns.

   This runs in O(n) time because mergepairs is called in a sequence like so n/2 + n/4 + n/8 ... + 1.


   c) 
   
   Replacing makepass with a fold

   foldl (\acc h -> mergePairs (h:acc)) [] heaps

   Replacing mergePairs and makePass?

   foldl (\acc h -> if length acc == 0 then (h:acc) else let (h2:acc) = acc in (merge (h:h2)) ++ acc) [] heaps   

   d) 

   	t1 = insert 1 $ insert 5 $ insert 3 $ insert 8 E

    T 1 8 E E
    T 1 3 (T 1 8 E E) E
    T 2 3 (T 1 8 E E) (T 1 5 E E )
    T 1 2 (T 2 3 (T 1 8 E E) (T 1 5 E E)) E (Final Result)

	t2 = insert 8 $ insert 6 $ insert 4 $ insert 2 E

    T 1 2 E E
    T 1 2 (T 1 4 E E) E
    T 2 2 (T 1 4 E E) (T 1 6 E E)
    T 2 2 (T 1 4 E E) (T 1 6 (T 1 8 E E) E) (Final result)

    e) findMin and deleteMin were not implemented in LHeaps. Implement
       them. Assume you can import LHeaps
 
        findmin simply returns the root 

        findMin :: Heap a -> Maybe a
        findMin h = case h of
                    E -> Nothing
                    (T _ x _ _ ) -> Just x

        deleteMin deletes the root and merges the two subtrees

        deleteMin (T _ _ E E) = E
        deleteMin (T _ _ a b) = merge a b




Question 2: Binomial heaps 
a) What is a rank in this implementation of BinomialHeaps? (hw5)

Answer:
Rank is an int that signifies how many nodes are in the tree. 
A rank 0 has 1 node a rank 1 two nodes and so on this is analgous to how binary numbers are.

b) Using the representation in BinomialHeaps.lhs, what would be the
result of the following expression (do not run this in the Haskell
interpreter)

insert 5 $ insert 3 $ insert 2 $ insert 1 empty

Answer:
WrapHeap [(0,Node 1 [])]
Wrapheap [(1,Node 1 [Node 2 []])]
WrapHeap [(0,Node 3 []), (1,Node 1 [Node 2 []])]
WrapHeap [(2,Node 1 [Node 3 [Node 5 []], Node 2 []])]

c) In BinomialHeaps.lhs explain how the representation maintains the
Heap property. In particular, explain how insert uses link and insertTree, 


Insert starts the process by simply calling insertTree with the element in an internal heap of rank 0

insertTree starts from the least signficiant binary value checks if the rank is less or equal to the current tree. 
If its less it simply adds the tree to the heap. If they are equal then the two trees are linked. Linking is what helps maintain the
heap property within each indivdual tree by making the root be the minimum element in each tree.


e) hw5 asks you to use a different representation, keeping only one rank for
each tree.  In the representation BHeaps (hw5), the InternalHeap is just a
list of (Rank,Tree) pairs. Each Tree in such a pair is a Tree
as defined in a binomial queue. where the rank indicates
what kind of tree it is. In the original representation
the rank was kept at the Node level, which means that we replicated
the rank throughout the heap.  So now a pair just
has 1 Rank: the number it represents instead of a rank for
each Node.  Rank will be the exponent for 2 that this tree
represents. So a rank of 0 is 2^0.

	data Tree = Node Int [Tree]
		deriving (Show, Eq)
	type Rank = Int
	type InternalHeap = [(Rank, Tree)]
	data Heap = WrapHeap InternalHeap
		deriving (Show,Eq)

Here is an example InternalHeap:

	t1 :: [(Rank, Tree)]
	t1 = [(0, Node 1 []), (1, Node 2 [Node 3 []]), (2, Node 3 [Node 4 [Node 5 [Node 6 []]]])]

To insert (or merge) a new node, we have to find its spot
eg if it is rank r, we need to find the rank r tree, if it exists.
If it does not exist, we just insert (r,t) at that point.
If it does exist, then we add them, creating a tree of rank r+1,
and then continue to insert/merge

	t2 :: [(Rank, Tree)] -- InternalHeap
	t3 :: [(Rank, Tree)] -- InternalHeap
	t2 = [(0, Node 1 []), (1, Node 2 [Node 3 []])]
	t3 = [(0, Node 2 []), (2, Node 3 [Node 4 [Node 5 [Node 6 []]]])]

Now t2 contains a rank 0 and a rank 1
t3 contains a rank 0 and a rank 2

When I add them, I will create a new rank 1 node:

	t4 = [(1, Node 1 [Node 2 []])]

and I will merge the remaining rank 1 and rank 2:

	t5 = [(1, Node 2 [Node 3 []]), (2, Node 3 [Node 4 [Node 5 [Node 6 []]]])]

Give the algorithm to merge two InternalHeaps.
You will also need an add to add two Nodes of the same rank.

Answer: 

So heres my code that I did for hw5

>	merge :: Ord a => Heap a -> Heap a -> Heap a
>	merge h1@(WrapHeap t) h2@(WrapHeap []) = h1
>	merge h1@(WrapHeap []) h2@(WrapHeap t)  = h2
>	merge h1@(WrapHeap ts1'@(t1:ts1)) h2@(WrapHeap ts2'@(t2:ts2)) = if (rank t1) < rank t2 then t1Less
>                                                                                         else if (rank t2) < (rank t1) then t1More             
>                                                                                         else heapsEqual
>                                                                   where t1Less     = (WrapHeap (t1:(getIHeap $ merge  (WrapHeap ts1) (WrapHeap ts2'))))
>                                                                         t1More     = WrapHeap (t2:(getIHeap $ merge  (WrapHeap ts1') (WrapHeap ts2)))
>                                                                         heapsEqual = WrapHeap (insertTree (link t1 t2) (getIHeap $ merge (WrapHeap ts1) (WrapHeap ts2))) 


It is a little clunky because I worked around the representation alot.

Essentially this walks down the two heaps and checks the rank of each tree in the respective heaps.

It has a different case for when the first tree is less, more, or equal to the tree in the second heap.

both t1Less and t1More do around the same thing.

when the two trees are equal the trees are linked and then inserted into the result of merge being called on the rest of the respective heaps.


Question 3: In the ExplicitMin homework (hw5) there is a comment:
-- NOTE: without functors or type classes, we would manually swap in
-- different implementations of H by twiddling the following imports

a) How do type classes avoid this manual twiddling?

Because you could make BHeaps and LHeaps an instance of a Heap typeclass so it would make creating a datatype and functions for the explicitmin module easier.

We can make any heaps we want to use an instance of the heap typeclass and this will make creating the datatype and functions much more flexible

b) Give a type class and instance declaration that would accomplish
this. 

> import qualified BHeaps as B
> import qualified LHeaps as L



> class Heap h where
>   empty     :: Ord a =>  h a
>   isEmpty   :: Ord a =>  h a -> Bool
>   findMin   :: Ord a => h a -> Maybe a
>   insert    :: Ord a => a  -> h a -> h a
>   deleteMin :: Ord a => h a  -> Maybe (h a)
>   merge     :: Ord a => h a  -> h a  -> h a

> data Heap a = E | NE a h deriving (Show)

> class EMHeap h  where
>   empty     :: (Ord a, Heap h) =>  h a h
>   isEmpty   :: (Ord a, Heap h) =>  h a h -> Bool
>   findMin   :: (Ord a, Heap h) => h a h -> Maybe a 
>   insert    :: (Ord a, Heap h) => a  -> h a h -> h a h
>   deleteMin :: (Ord a, Heap h) => h a h  -> Maybe (h a h)
>   merge     :: (Ord a, Heap h) => h a h  -> h a h  -> h a h

> instance Heap (B.Heap) where
>     empty = B.empty
>     isEmpty = B.isEmpty
>     merge   = B.merge
>     insert = B.insert
>     findMin = B.findMin
>     deleteMin = B.deleteMin 

> instance Heap (L.Heap) where
>     empty = L.empty
>     isEmpty = L.isEmpty
>     merge   = L.merge
>     insert = L.insert
>     findMin = L.findMin
>     deleteMin = L.deleteMin 


c) what advantage do typeclasses have for reuse?

Type classes allow you to use the same functions for different representations. If something is an instance of a typeclass you can rest assured it has certain behaviour so you can
call a function that is in the typeclass without having to rewrite a different function for every single different type.

In our case if we make a general Heap typeclass we dont need to create different functions and representations for our ExplicitMin type.



Question 4:  Red-black trees (I know you haven't completed the code for
red black trees yet)

a) What is a rotation in a red-black tree?
A rotation is used to fix red-red violations there are four cases of these depending on the ordering of the violation
LL, RR, LR, RL



b) What is the black-height ?
The black height is the number of nodes in a path to an empty node that are labeled black. 


c) draw the trees that result from the 4 cases shown in the lecture notes
and in the code:

balance : Color -> Tree -> Int -> Tree -> Tree
balance c l val r =
  case (c, l, val, r) of
    (B, T R (T R a x b) y c, z, d) -> T R (T B a x b) y (T B c z d)
    (B, T R a x (T R b y c), z, d) -> T R (T B a x b) y (T B c z d)
    (B, a, x, T R (T R b y c) z d) -> T R (T B a x b) y (T B c z d)
    (B, a, x, T R b y (T R c z d)) -> T R (T B a x b) y (T B c z d)
    _                              -> T c l val r


Im not sure if you meant there were specific trees in the slides so I will just show trees that result from the cases show in the code and the lecture slides

Case LR
  cb
 /  \
 ar d  
 /\
 r br
   / \
   f  x 
  
    bb
   / \
 ar    cr
 / \   / \
 r  f  x   d

Case LL

   ab
  /  \
  xr  e
 / \
 dr c
 / \
 q  p

    xb
   /  \
  dr   ar
 / \  /  \
 q  p c   e


Case RR

 ab
/  \
f  xr
  /  \
  b   dr
     /  \
    q    p

  xb
 /  \
 ar   dr
 /\  /  \
 f b q   p

 Case RL

   xb
  /  \
 f    dr
     /  \
    tr   a
   /  \
   p   q


   tb
  /   \
 xr    dr
/  \  /  \
f   p q   a


In the cases of the lecture slides I only saw three cases they used those are as follows

Case 1

   tb
  /  \
 ar  dr
   \
    br

Recolors a and d then recurses up tree

    tr
   /  \
  ab  db
   \
    br

Case 2

   cb
  /
  ar
    \
    br

Left rotate to transform to case 3


Case 3

   cb
  /
  br
 /
ar
 
right-rotate

   bb
  /  \
 ar  cr










d) In the lecture slides for the red-black trees there is a (case 1)
that is not present in this (Haskell) code. What role does (case 1) have?
Why don't we have a case 1 here? How do these Haskell rotations differ
from the lecture slides?

The code in the lecture slides is more imperative and has to rotate the trees in part because the lack of pattern matching.

I think they use the recoloring case because they are not recursing down the tree like we do in haskell. In the code in the slides they are going down to the insert then after inserting
they recurse up the tree but not down the tree like we do in our code. Doing case 1 does not create a problem with black height in the two subtrees so they dont have to recurse down the tree.

Essentially their code is working in a different direction they create a possible RR violation upwards we create a possible RR violation downwards.

Their rotations differ because they are working up the tree as I said before they insert and they put the violation above instead of below.

