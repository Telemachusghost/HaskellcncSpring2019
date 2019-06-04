>	module EnumerateRB where
>	import qualified Data.List as L

In the original ELM this version used the lazy list implementation
in ELM. But we will just use the regular Haskell lazy lists.
This isn't quite an exact inverse: the ELM lazy lists implemented
laziness using functions that they explicitly forced to evaluate
when needed. Haskell handles that in its evaluation policy.
Haskell's strict lists also do a fair amount of the work for us
by making the data structure itself eager, thus switching
the evaluation policy.

You might experiment with using the strict Haskell lists to see
what impact that has on performance.



------------------------------------------------------------------------------

>	data Color = R | B
>		deriving (Show, Eq)
>	data Tree  = E | T Color Tree Int Tree
>		deriving (Show, Eq)

>	check bh t =
>	  color t == B && noRedRed t && blackHeight t == Just bh

>	color t = case t of {T c _ _ _ -> c; E -> B}

>	okBlack t = case blackHeight t of {Just _ -> True; Nothing -> False}

>	blackHeight t = case t of
>	  E -> Just 0
>	  T c l _ r ->
>	    let i = case c of {B -> 1; R -> 0} in
>	    case (blackHeight l, blackHeight r) of
>	      (Just n, Just m) -> if ( n == m) then Just (i + n)
>	        		    else  Nothing
>	      _ -> Nothing

>	noRedRed t = case t of
>	  E                   -> True
>	  T R E _ E           -> False
>	  T R (T R _ _ _) _ _ -> False
>	  T R _ _ (T R _ _ _) -> False
>	  T _ l _ r           -> noRedRed l && noRedRed r
    

	  T _ l _ r           -> List.all noRedRed [l, r]

------------------------------------------------------------------------------

>	rbTrees :: Int -> [Tree]
>	rbTrees 0  = [E]
>	rbTrees bh = vTrees
>	             where
>	             vTrees = validTrees bh (rbTrees' bh)

>	validTrees bh trees = foldl (\acc x -> if (check bh x) then x:acc else acc) [] trees

------------------------------------------------------------------------------

Yeah, I am just having trouble figuring out how to get the right cases and form the correct trees with the caveats required.


>	rbTrees' :: Int -> [Tree]
>	rbTrees' 0  = [E]
>	rbTrees' bh = validTrees bh allTrees 
>	              where 
>	              allTrees = concatMap (\x -> buildTrees x (rbTrees' (bh - 1))) oneBlack
>	              oneBlack = [(T B E 0 E), (T B (T R E 0 E) 0 (T R E 0 E)), (T B (T R E 0 E) 0 E), (T B E 0 (T R E 0 E))]

>	cartProdWith f xs ys = concat $ map (\x -> foldF x ys) xs 
>	                       where
>	                       foldF x ys2 =(foldr (\y acc ->  (f (x,y)):acc) [] ys2)                      

So basically I will just work through the cases with this function buildTrees in order to get all the permutations

>	buildTrees t@(T B E n E) ts = newTree [t] ts

>	buildTrees t@(T B t2@(T R E n2 E) n E) ts = allTrees 
>	                                            where
>	                                            newTreesL   = newTree [t2] ts
>	                                            rootTreesL  = cartProdWith newTreeL [(T B E n E)] newTreesL
>	                                            allTrees    = cartProdWith newTreeR rootTreesL ts  

>	buildTrees t@(T B E n t2@(T R E n2 E)) ts = allTrees 
>	                                            where
>	                                            newTreesR   = newTree [t2] ts
>	                                            rootTreesR  = cartProdWith newTreeR [(T B E n E)] newTreesR
>	                                            allTrees    = cartProdWith newTreeL rootTreesR ts

>	buildTrees t@(T B t2@(T R E n2 E) n t3@(T R E n3 E)) ts = allTrees 
>	                                                          where
>	                                                          newTreesL   = newTree [t2] ts
>	                                                          newTreesR   = newTree [t3] ts
>	                                                          rootTreesL  = cartProdWith newTreeL [(T B E n E)] newTreesL
>	                                                          allTrees    = cartProdWith newTreeR rootTreesL newTreesR

>	newTree t ts = trees
>	               where
>	               lts = cartProdWith newTreeL t ts
>	               trees = cartProdWith newTreeR lts ts

>	newTreeL ((T c E n r),ex) = (T c ex n r) 

>	newTreeR ((T c l n E),ex) = (T c l n ex)


Sanity Checks

*EnumerateRB> rbTrees 0
[E]
(0.00 secs, 59,856 bytes)

*EnumerateRB> rbTrees 1
[T B E 0 E]
(0.00 secs, 89,496 bytes)


*EnumerateRB> map  (length . rbTrees) [0..3]
[1,1,4,400]
(0.07 secs, 16,077,256 bytes)

 



