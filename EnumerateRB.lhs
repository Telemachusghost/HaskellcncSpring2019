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
>	  T R (T R _ _ _) _ _ -> False
>	  T R _ _ (T R _ _ _) -> False
>	  T _ l _ r           -> noRedRed l && noRedRed r

	  T _ l _ r           -> List.all noRedRed [l, r]

------------------------------------------------------------------------------

>	rbTrees :: Int -> [Tree]
>	rbTrees 0  = [E]
>	rbTrees bh = validTrees
>	             where
>	             validTrees = foldl (\acc x -> if (check bh x) then x:acc else acc) [] (rbTrees' (bh))


------------------------------------------------------------------------------

I would like to see if there is a better way to not have so many intermediary data structures.

Having difficulty getting it to work correctly after black height 2
Maybe I should build the tree from the bottom up?

Ok, I think this is working correctly now. I can calculate 



>	rbTrees' :: Int -> [Tree]
>	rbTrees' 0  = [E]
>	rbTrees' bh = allTrees 
>	              where 
>	              diffL = cartProdWith newTreeL oneBlack ((rbTrees' (bh - 1)))
>	              allTrees = cartProdWith newTreeR diffL (rbTrees' (bh - 1))
>	              oneBlack = [(T B E 0 E), (T R (T B E 0 E) 0 (T B E 0 E))]

>	cartProdWith f xs ys = concat $ map (\x -> foldF x ys) xs 
>	                       where
>	                       foldF x ys2 =(foldr (\y acc ->  (f (x,y)):acc) [] ys2)                      

>	newTreeL ((T c E n r),ex) = (T c ex n r)
>	newTreeL ((T c (T c2 l n2 r2) n (T c3 l2 n3 r3)),ex) = (T c (T c2 ex n2 r2) n (T c3 ex n3 r3))
>	newTreeL ((T c (T c2 l n2 r) n E),ex) = (T c (T c2 ex n2 r) n E)

>	newTreeR ((T c l n E),ex) = (T c l n ex)
>	newTreeR ((T c (T c2 l n2 r) n (T c3 l2 n3 r2)),ex) =  (T c (T c2 l n2 ex)  n (T c3 l2 n3 ex))
>	newTreeR ((T c E n (T c2 l n2 r)),ex) =  (T c E  n (T c2 l n2 ex))

Sanity Checks

*EnumerateRB> rbTrees 0
[E]
(0.00 secs, 59,856 bytes)

*EnumerateRB> rbTrees 1
[T B E 0 E]
(0.00 secs, 89,496 bytes)


*EnumerateRB>  map  (length . rbTrees) [0..3]
[1,1,4,64]
(0.01 secs, 1,255,640 bytes)

 



