> import qualified Data.List as L

Part 0:

> data Tree a = Leaf a | Fork (Tree a) (Tree a) deriving (Show, Eq)
> type Forest a = [Tree a]

> cost :: (Ord a, Num a) => Tree a -> a
> cost (Leaf a) = a
> cost (Fork u v) = 1 + mx
>                   where
>                   mx = max (cost u) (cost v)

> trees :: (Ord a, Num a) => [a] -> [Tree a]
> trees[x] = [Leaf x]
> trees (x:xs) = concatMap (prefixes x) (trees xs) 

> prefixes :: (Ord a, Num a) => a -> Tree a -> [Tree a]
> prefixes x t@(Leaf y) = [Fork (Leaf x) t]
> prefixes x t@(Fork u v) = [Fork (Leaf x) t] ++ [Fork u' v | u' <- prefixes x u]

> insert' x ts = Leaf x : split' x ts

> split' x [u] = [u]
> split' x (u:v:ts) = if max x (cost u) < cost v then u : v : ts
>                    else split' x (Fork u v : ts)

> minByCost :: (Ord a, Num a) => [Tree a] -> Tree a
> minByCost ts =  minTree
>                 where minTree = ts !! minCostIndex
>                       costTrees = map cost ts
>                       minCost = minimum costTrees
>                       Just minCostIndex =   L.elemIndex minCost costTrees

> mincostTree' :: (Ord a, Num a) => [a] -> Tree a
> mincostTree' = minByCost . trees

Timing: 

*Main> minByCost (trees [1..5])
Fork (Fork (Fork (Fork (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4)) (Leaf 5)
(0.01 secs, 151,328 bytes)

*Main> minByCost (trees [1..10])
Fork (Fork (Fork (Fork (Fork (Fork (Fork (Fork (Fork (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4)) (Leaf 5)) (Leaf 6)) (Leaf 7)) (Leaf 8)) (Leaf 9)) (Leaf 10)
(0.10 secs, 20,870,816 bytes)

insert' 5 (trees [1,2,3,4])
[Leaf 5,Fork (Fork (Fork (Fork (Fork (Leaf 1) (Fork (Leaf 2) (Fork (Leaf 3) (Leaf 4)))) (Fork (Fork (Leaf 1) (Leaf 2)) (Fork (Leaf 3) (Leaf 4)))) (Fork (Leaf 1) (Fork (Fork (Leaf 2) (Leaf 3)) (Leaf 4)))) (Fork (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) (Leaf 4))) (Fork (Fork (Fork (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4))]
(0.02 secs, 342,672 bytes)
                          
Part 1:


> prefixes' :: (Ord a, Num a) => a -> Forest a -> [Forest a]
> prefixes' x ts = [Leaf x : rollup (take k ts) : drop k ts
>                 | k <- [1..length ts]]

> forests :: (Num a, Ord a) => [a] -> [Forest a]
> forests  = foldrn (concatMap.prefixes') (wrap.wrap.Leaf)

> foldrn :: (a->b->b) -> (a->b) -> [a] -> b
> foldrn f g [x] = g x
> foldrn f g (x:xs) = f x (foldrn f g xs)

> wrap :: a -> [a]
> wrap x = [x]

> rollup :: (Ord a, Num a) => Forest a -> Tree a
> rollup = foldl1 Fork

> trees' :: (Ord a, Num a) => [a] -> [Tree a]
> trees' = map rollup . forests

> minBy f = foldl1 (cmp f)
> cmp f u v = if f u <= f v then u else v

> cost' :: (Ord a, Num a) => Tree a -> a
> cost' (Leaf x) = x
> cost' (Fork u v) = 1 + (cost' u `max` cost' v) 

> treeTD :: (Num a, Ord a) => [a] -> Tree a
> treeTD [a] = Leaf a
> treeTD l = let (l1,l2) = splitAt (length l `div` 2) l
>            in Fork (treeTD l1) (treeTD l2)

> treeBU :: (Num a, Ord a) => [a] -> Tree a
> treeBU = fromTrs . map Leaf

> fromTrs :: (Num a, Ord a) => [Tree a] -> Tree a
> fromTrs [t] = t
> fromTrs t = fromTrs (step t)
>             where
>             step (t1:t2:ts) = (Fork t1 t2) : (step ts)
>             step ts = ts

> mincostTree :: (Ord a, Num a) => [a] -> Tree a
> mincostTree = minBy cost' . trees'

> insert x ts  = Leaf x : split x ts

> split x [u] = [u]
> split x (u:v:ts) = if  max x (cost u) < cost v then u : v : ts
>                    else split x (Fork u v : ts)

a) Given the list [1,3,2,5,4] how many possible trees are there?
(you don't have to calculate this, but you can use the above
functions to find out).

*Main> length $ trees' [1,3,2,5,4]
14
(0.01 secs, 71,864 bytes)

b) For the same list, what are all the costs? (again use the above
functions)?

*Main> map cost' (trees' [1,3,2,5,4])
[7,7,8,8,8,9,9,8,9,7,7,8,8,9]
(0.01 secs, 123,192 bytes)

c)  Compare the result of treeBU and treeTD for the above list. Draw
the results.  Are they the same tree? Do they have the same cost?

*Main> treeBU [1,3,2,5,4]
Fork (Fork (Fork (Leaf 1) (Leaf 3)) (Fork (Leaf 2) (Leaf 5))) (Leaf 4)
(0.01 secs, 118,760 bytes)

*Main> treeTD [1,3,2,5,4]
Fork (Fork (Leaf 1) (Leaf 3)) (Fork (Leaf 2) (Fork (Leaf 5) (Leaf 4)))
(0.01 secs, 118,840 bytes)


Bottom Up tree

           *
         /   \
        *     4
       / \   
       *  *  
      /\  /\
     1 3  2 5


Top Down Tree

        *
       /  \
      /    \
     *      *
    / \    / \
    1  3  2   *
             / \
            5   4


Top down seems more balanced which makes sense because of how it divides up the list.

bottom up puts more nodes on the left because of how it just forks leafs until there is only one other tree left.
I think it's interesting to see how you can see the algorithm in the different trees.


They have the same cost

*Main> cost $ treeBU [1,3,2,5,4]
8
(0.01 secs, 62,512 bytes)
*Main> cost $ treeTD [1,3,2,5,4]
8
(0.01 secs, 63,032 bytes)


Part 2:

a) Use :set +s to time the mincostTree. Make trees from
[1..5], [1..10], and [1..14]

*Main> mincostTree [1..5]
Fork (Fork (Fork (Fork (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4)) (Leaf 5)
(0.01 secs, 180,104 bytes)
*Main> mincostTree [1..10]
Fork (Fork (Fork (Fork (Fork (Fork (Fork (Fork (Fork (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4)) (Leaf 5)) (Leaf 6)) (Leaf 7)) (Leaf 8)) (Leaf 9)) (Leaf 10)
(0.19 secs, 40,353,880 bytes)
*Main> mincostTree [1..14]
Fork (Fork (Fork (Fork (Fork (Fork (Fork (Fork (Fork (Fork (Fork (Fork (Fork (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4)) (Leaf 5)) (Leaf 6)) (Leaf 7)) (Leaf 8)) (Leaf 9)) (Leaf 10)) (Leaf 11)) (Leaf 12)) (Leaf 13)) (Leaf 14)
(42.05 secs, 8,526,373,736 bytes)

*Main> mincostTree' [1..14]
Fork (Fork (Fork (Fork (Fork (Fork (Fork (Fork (Fork (Fork (Fork (Fork (Fork (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4)) (Leaf 5)) (Leaf 6)) (Leaf 7)) (Leaf 8)) (Leaf 9)) (Leaf 10)) (Leaf 11)) (Leaf 12)) (Leaf 13)) (Leaf 14)
(21.12 secs, 4,299,143,656 bytes)

*Main> mincostTree' [1..14] == mincostTree [1..14]
True
(61.68 secs, 12,825,105,144 bytes)

*Is the implementation I did in part 0 supposed to be so much faster and more efficient?*

b) The author claims that the insertions can be done in
linear time. To test this claim time the insertions
using foldrn insert (wrap.Leaf) on lists
of sizes 100, 1000, 10000.

Sizes   Time
100     0.04 secs
1000    0.31 secs
10000   3.6 secs

It seems about right 10*0.31 = 3.1

c) Do the rewriting on 

forests [1,2,3]
foldrn (concatMap.prefixes) (wrap.wrap.Leaf) [1,2,3]
(concatMap.prefixes) 1 (foldrn (concatMap.prefixes) (wrap.wrap.Leaf) [2,3])



d) how are trees and forests related? draw a picture that shows
how a forest is turned into a list of trees.
(use on trees based on [1,2,3]

A forest is a list of trees.

So the forest is a list of trees with trees based on [1,2,3]

We have [[Leaf 1, Leaf 2, Leaf 3], [Leaf 1, Fork Leaf 2 Leaf 3]]

As described in the text each forest represents the spine of a tree or the sequence of right subtrees
along the path from leftmost leaf to the root. Rolling up the spine creates the tree because we are forking the
the right subtrees as we travel up the tree.

For ex. [Leaf 1, Leaf 2, Leaf 3]

Means the right subtrees are Leaf 1, Leaf 2, and Leaf 3 respectively

rolling them up produces Fork (Fork (Leaf 1) (Leaf 2)) Leaf 3 which is exactly what you would expect given that
description. 

Here is a drawing


1

 *
/ \
1 2

   *
  / \
 *   3
/ \
1  2



Part 3:

> type Tree' a = (a, Tree a)
> type Forest' a = [Tree' a]

> prefixes'' :: (Ord a, Num a) => a -> Forest' a -> [Forest' a]
> prefixes'' x ts = [(x,Leaf x) : rollup' (take k ts) : drop k ts
>                 | k <- [1..length ts]]

> forests' :: (Num a, Ord a) => [a] -> [Forest' a]
> forests'  = foldrn (concatMap.prefixes'') (wrap.wrap.(\x -> (x, Leaf x)))

> rollup' :: (Ord a, Num a) => Forest' a -> Tree' a
> rollup' = foldl1 (\(x,y) (x2,y2) -> ( (max x x2) + 1, Fork y y2))

> trees'' :: (Ord a, Num a) => [a] -> [Tree' a]
> trees'' = map rollup' . forests'

> cost'' :: (Ord a, Num a) => Tree' a -> a
> cost'' (x1,Leaf x) = x1
> cost'' (x,Fork u v) = x 

> mincostTree'' :: (Ord a, Num a) => [a] -> Tree' a
> mincostTree'' = minBy cost'' . trees''

> insert'' x ts  = (x, Leaf x) : split'' x ts

> split'' x [u] = [u]
> split'' x (tu@(cl,u):tv@(cr,v):ts) = if  max x cl < cr then tu : tv : ts
>                                      else split'' x ((1+ max cl cr,Fork u v) : ts)


Size           Time
100  (0.05 secs, 1,128,648 bytes)
1000 (0.47 secs, 12,097,360 bytes)
10000 (5.67 secs, 135,363,080 bytes)

So this is actually slower Which I think is because of dealing with so many tuples I am thinking in a real world situation where the list has forks this would be faster.

So I compared insert and insert'' with inserting 4 into the trees provided by trees [1,2,3,4,5,4]

insert: (0.14 secs, 5,193,056 bytes) 
insert'': (0.12 secs, 3,279,584 bytes)

Ok this seems more correct. Now with with more trees.

trees [1,2,3,6,8,0,2,90]

insert: (2.63 secs, 300,903,984 bytes)
insert'': (1.67 secs, 46,617,560 bytes)

That is a lot better. Also bigger lists which wouldn't even run on my laptop with the old representation were able to run with the new representation.



Attempt at Extra credit Dynamic Programming:

> prfxs list  = L.inits list `zip` L.tails list 

> minT []     = []
> minT (a:[]) = [Leaf a]
> minT (a:b:[]) = [Fork (Leaf a) (Leaf b)]
> minT (a:b:xs) = [Fork (Leaf a) ]



> minDP list = 
>             where
>             n = length list
>             f = minT 
>             r = listArray (0,n) (map f (prfxs list))


