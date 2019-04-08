> import ListsAndTrees
> import Control.Applicative
> import Data.List
> import Data.Set

1.

This is O(n) time because we are doing one drop for each element in the list
It is O(n) space because the resulting list has n+1 elements so O(n)

> suffix :: [a] -> [[a]]
> suffix [] = [[]]
> suffix list = list:suffix (drop 1 list)

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

> balancedTrees' :: Int -> Int -> [Tree]

> balancedTrees' x n  = let (q,r) = quotRem n 2
>                       in if r == 1 then let leftSide  = [q]
>                                             rightSide = [q]
>                                             leftTree  = balancedTree' x <$> leftSide
>                                             rightTree = balancedTree' x <$> rightSide
>                                             combinedSubtrees = zip leftTree rightTree
>                                             in [Node x l r | (l,r) <- combinedSubtrees]
>                         else let leftSide  = [(q-1), (q)]
>                                  rightSide = [(q), (q-1)]
>                                  leftTree  = balancedTree' x <$> leftSide
>                                  rightTree = balancedTree' x <$> rightSide
>                                  combinedSubtrees = zip leftTree rightTree
>                                  in [Node x l r | (l,r) <- combinedSubtrees]  


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
>                               nodesDelete = (take (nNodes-n) $ repeat False) ++ (take n $ repeat True)
>                                                                     


function to trim rightmost node of complete tree takes the height of tree 


> trimTree :: Int -> Int -> [Bool] -> Tree  -> Tree 
> trimTree _ _ _ Empty = Empty
> trimTree n parent list (Node x l r) | n == 1 = if rVal && lVal then Node x Empty Empty 
>                                                                else if rVal then Node x l Empty 
>                                                                             else  if lVal then Node x Empty r else Node x l r                                                                            
>                                     | otherwise = Node x (trimTree (n-1) lChild list l) (trimTree (n-1) rChild list r)   
>                                       where
>                                       lChild = 2*parent+1
>                                       rChild = 2*parent+2
>                                       lVal = list !! lChild
>                                       rVal = list !! rChild


 
6. 

I will use a permutation to get all different ways of assembling the bool flag array 


> permNodes nodes = fromList (permFlags 1 nodes)

> permFlags :: Int -> Int  -> [[Bool]]
> permFlags n nd   | nd == 1 = [take n $ repeat False]
>                  | otherwise = permutations list ++ permFlags (n+1) (nd-1)
>                                where falseList = take ((nd-1)) $ repeat False
>                                      trueList = take n $ repeat True
>                                      list = falseList ++ trueList 

Now I can just map trimTree to the resulting permutation to get all permutations


Your sanity check essentially suceeds I think I did my fullTree a litle differently than yours possibly.

My algorithm is probably inefficent so I was not able to check the 65k result on my laptop. but 1, 3, 15, 255 all suceeded 

> almostCompleteTrees' x 0 = [Node x Empty Empty] 
> almostCompleteTrees' x h = trees
>                           where boolFlags = toList $ permNodes (2^h) 
>                                 nodeList  = Data.List.map ( (take (2^(h)-1)  $ repeat False)  ++) boolFlags
>                                 tree      = fullTree' x h
>                                 trees     = Data.List.map (\x -> trimTree h 0 x tree) nodeList








