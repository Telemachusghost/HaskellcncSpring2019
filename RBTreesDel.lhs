>	module RBTreesDel (Set, RBTreesDel.empty, member, RBTreesDel.insert, delete, size) where

Note that I specialized which empty and insert to export in the
module line ... this could be avoided by using type classes, and makeing
this an instance so the correct "empty" and "insert" are selected
by the instance declaration, but this is a quicker way to get a proof
of concept off the ground.

>	import qualified RBMaps as Map 

Note this file was derived from an elm implementation
that used comparable as a constraint on the type
for Map, which I replaced with an Ord constraint.

>	data Set v k = Set (Map.Map v Bool ) 

>	empty :: (Ord v) => Set v Bool
>	empty = Set Map.empty 
>	  

>	member :: (Ord v) => v -> Set v k -> Bool
>	member v (Set m) = case Map.get v m of
>                      Just True    -> True
>                      _            -> False

>	insert :: (Ord v) => v -> Set v Bool -> Set v Bool
>	insert v   (Set m) = Set newMap
>                        where 
>                             newMap = Map.insert v True m 

*RBTreesDel> member "d" $ insert "d" 2 empty
True
(0.01 secs, 71,448 bytes)

*RBTreesDel> member "x" $ insert "d" 2 empty
False
(0.01 secs, 72,200 bytes)

>	delete :: (Ord v) => v -> Set v Bool -> Set v Bool
>	delete v s@(Set m) = case member v s of
>	                     True -> newS 
>	                     False -> s
>	                     where newS = Set (Map.insert v False m)

Map.keys is O(n) both foldl's are O(n) so total function is O(n)

>	size :: (Ord v) => Set v k -> (Int, Int)
>	size (Set m) = (trueCount,falseCount)
>	               where 
>	                    keyList = Map.keys m
>	                    trueCount = foldl (\acc k -> if k == True then acc+1 else acc) 0 keyList
>	                    falseCount = foldl (\acc k -> if k == False then acc+1 else acc) 0 keyList


Testing

*RBTreesDel> let s = insert 4 $ insert 5 $ insert 20 $ insert (-5) empty
(0.00 secs, 32,880 bytes)
*RBTreesDel> member 4 s
True
(0.01 secs, 77,632 bytes)
*RBTreesDel> member 5 s
True
(0.01 secs, 76,888 bytes)
*RBTreesDel> member 20 s
True
(0.01 secs, 77,176 bytes)
*RBTreesDel> member (-5) s
True
(0.01 secs, 76,880 bytes)


*RBTreesDel> let r = delete (-5) s
(0.00 secs, 32,880 bytes)
*RBTreesDel> member (-5) r
False
(0.01 secs, 80,304 bytes)

*RBTreesDel> size r
(3,1)