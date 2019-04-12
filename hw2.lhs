> import ListsAndTrees
> import Control.Applicative
> import Data.List
> import Data.Set

1.

This is O(n) time because we are doing one Data.List.drop for each element in the list
It is O(n) space because the resulting list has n+1 elements so O(n)

> suffix :: [a] -> [[a]]
> suffix [] = [[]]
> suffix list = list:suffix (Data.List.drop 1 list)

2. 

> mem2 :: Int -> Tree -> Bool
> mem2 elem Empty = False
> mem2 elem (Node node leftTree rightTree) | elem == node = True
>                                          | otherwise = if elem < node then mem2 elem leftTree else mem2 elem rightTree

3. 

> fullTree' :: Int -> Int -> Tree
> fullTree' x h | h == 0 = Node x Empty Empty
>               | otherwise = (Node x newTree newTree)
>               where newTree = fullTree' x (h-1)


4.




> balancedTree' :: Int -> Int -> Tree
> balancedTree' _ 0 = Empty
> balancedTree' x n = let (q, r) = quotRem n 2
>                    in if r == 1 then let t = balancedTree' x q
>                                      in Node x t t
>                       else Node x (balancedTree' x (q-1)) (balancedTree' x q)

This is incorrect it does not give all permuations of the trees
could use a permuation function to permutate trees by swapping around trees

> balancedTrees' :: Int -> Int -> [Tree]

> balancedTrees' x n  = let (q,r) = quotRem n 2
>                       in if r == 1 then let leftSide  = [q]
>                                             rightSide = [q]
>                                             leftTree  = balancedTree' x <$> leftSide
>                                             rightTree = balancedTree' x <$> rightSide
>                                             combinedSubtrees = zip leftTree rightTree
>                                             in [Node x l r | (l,r) <- combinedSubtrees]
>                                    else let leftSide  = [(q-1), (q)]
>                                             rightSide = [(q), (q-1)]
>                                             leftTree  = balancedTree' x <$> leftSide
>                                             rightTree = balancedTree' x <$> rightSide
>                                             combinedSubtrees = zip leftTree rightTree
>                                             in [Node x l r | (l,r) <- combinedSubtrees]  


5.

So this sets up a complete tree of height h then it uses a trimmer to trim from the right side to produce the other variations

 Wrapper to make it easier to use. I send completeTrees' leafs minus one so that we dont delete all the nodes from the last level

> completeTreesW x h = completeTrees' x h ( (2^h)-1 )

Creates a list of all the different complete trees think suffixes it starts with one node on left then works up to having a complete last level.

> completeTrees' :: Int -> Int -> Int -> [Tree]

> completeTrees' _ _ (-1) = []

> completeTrees' x h leafs = completeTrees'' x h leafs : completeTrees' x h (leafs-1) 

Makes the list of nodes to delete and sends it to trimTree

> completeTrees'' :: Int -> Int -> Int -> Tree
> completeTrees'' x h n = trimTree h 0 nodesDelete (fullTree' x h) 
>                         where nNodes = 2^(h+1)-1
>                               nodesDelete = (Data.List.take (nNodes-n) $ repeat False) ++ (Data.List.take n $ repeat True)
>                                                                     


function to trim rightmost node of complete tree Data.List.takes the height of tree 


> trimTree :: Int -> Int -> [Bool] -> Tree  -> Tree 
> trimTree _ _ _ Empty = Empty
> trimTree n parent list (Node x l r) | n == 1 = if rVal && lVal then Node x Empty Empty 
>                                                                else if rVal then Node x l Empty 
>                                                                             else  if lVal then Node x Empty r else Node x l r                                                                            
>                                     | otherwise = Node x (trimTree (n-1) lChild list l) (trimTree (n-1) rChild list r)   
>                                       where
>                                       lChild = 2*parent+1
>                                       rChild = 2*parent+2
>                                       last  = (length list)
>                                       lVal = lChild > last
>                                       rVal = rChild > last


 
6. 

I will use a permutation to get all different ways of assembling the bool flag array 

> subSeq :: [a] -> [[a]]
> subSeq [] = [[]]
> subSeq (x:xs) = [x:ys | ys <- subSeq xs] ++ subSeq xs

> removeEmpty [[]] = []  
> removeEmpty (x:xs) = x : removeEmpty xs 

> comb :: Int -> [a] -> [[a]]
> comb 0 _      = [[]]
> comb _ []     = []
> comb m (x:xs) = Data.List.map (x:) (comb (m-1) xs) ++ comb m xs


> permNodes nodes  = removeEmpty . subSeq . Data.List.take nodes $ repeat False

> permFlags :: Int -> Int  -> [[Bool]]
> permFlags n nd   | nd == 1 = [Data.List.take n $ repeat False]
>                  | otherwise = permutations list ++ permFlags (n+1) (nd-1)
>                                where falseList = Data.List.take ((nd-1)) $ repeat False
>                                      trueList = Data.List.take n $ repeat True
>                                      list =   falseList ++ trueList 

Now I can just map trimTree to the resulting permutation to get all permutations


Your sanity check essentially suceeds I think I did my fullTree a litle differently than yours possibly.

My algorithm is probably inefficent so I was not able to check the 65k result on my laptop. but 1, 3, 15, 255 all suceeded 
I think if I passed just the layer before the last and not have to do a whole tree traversal would speed this up significantly
I think the slowdown is mainly due to permnodes because it is creating a crazy amount of permutations most of which are not being used because they are repeats

> almostCompleteTreesW x h = almostCompleteTrees' x h tree
>                            where tree = fullTree' x h

> almostCompleteTrees' x 0 _ = [Node x Empty Empty] 
> almostCompleteTrees' x h tree = trees
>                                 where boolFlags =  permNodes (2^h) 
>                                       nodeList  = Data.List.map ( (Data.List.take (2^(h)-1)  $ repeat False)  ++) boolFlags
>                                       trees     = Data.List.map (\x -> trimTree h 0 x tree) nodeList








