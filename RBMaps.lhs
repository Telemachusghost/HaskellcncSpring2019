>	module RBMaps (Map, empty, get, insert, toList, keys) where

Here v must ORD type ... may need to make some changes
to conform to the comparable constraint in ELM.

>	data Color = R | B
>		deriving (Show)
>	data Map v k
>	   = E
>	   | T Color (Map v k) (v,k) (Map v k)
>		deriving (Show)

>	empty :: Ord v => Map v k
>	empty = E

>	get :: Ord v => v -> Map v k -> Maybe k
>	get _ E = Nothing
>	get v (T _ l (v2,k) r) = if v < v2 then get v l
>                            else if v > v2 then get v r
>                            else Just k   

*RBMaps> get 2 (T B E (2,"d") E)
Just "d"
(0.01 secs, 73,440 bytes)


>	insert :: Ord v => v -> k -> Map v k -> Map v k
>	insert v k t = let (T _ a y b) = ins v k t in (T B a y b)

>	ins :: Ord v => v -> k -> Map v k -> Map v k
>	ins v k t =  case t of
>	             E                    -> (T R E (v,k) E)
>	             t1@(T c a (v2,k2) b) -> if v < v2 then balance c (ins v k a) (v2,k2) b 
>	                                     else if v > v2 then balance c a (v2,k2) (ins v k b)
>	                                     else (T c a (v,k) b)
>	                                     

*RBMaps> insert (-1) 5 $ insert 90 1 $ insert 20 4 $ insert 2 5 $ insert 5 3 empty
T B (T B (T R E (-1,3) E) (2,3) E) (5,3) (T B E (20,3) (T R E (90,3) E))
(0.02 secs, 136,912 bytes)

*RBMaps> let m = insert "b" 2 $ insert "a" 1 $ empty
(0.01 secs, 32,880 bytes)
*RBMaps> get "c" $ insert "c" 3 m
Just 3
(0.02 secs, 78,104 bytes)
*RBMaps> get "b" $ insert "b" 3 m
Just 3
(0.01 secs, 76,600 bytes)


>	balance :: Ord v => Color
>	       -> Map v k -> (v,k)  -> Map v k
>	       -> Map v k
>	balance B (T R (T R a x b) y c) z d  = (T R (T B a x b) y (T B c z d)) -- Left Left Case
>	balance B (T R a x (T R b y c)) z d  = (T R (T B a x b) y (T B c z d)) -- Left Right case
>	balance B a q (T R b x (T R f r t))  = (T B (T R a q b) x (T R f r t)) -- Right Right case
>	balance B  a x (T R (T R y z t) p r) = (T B (T R a x y) z (T R y p r)) -- Right Left case
>	balance a b c d                      = (T a b c d)  

Really simple first attempt

>	toList :: Ord v => Map v k -> [(v,k)]
>	toList E = []
>	toList (T _ l (v,k) r) = leftList ++ [(v,k)] ++ rightList
>	                         where leftList = toList l
>	                               rightList = toList r  

Gives all the value bindings in inorder of the tree

>	keys :: Ord v => Map v k -> [k]
>	keys E = []
>	keys (T _ l (_,k) r) = leftList ++ [k] ++ rightList
>	                       where leftList = keys l
>	                             rightList = keys r

*RBMaps> toList m
[("a",1),("b",2)]


