>	import BSTTree

	import BSTTree2

	import BSTTree3

>	t1 :: BSTTree Integer

>	t1 = empty
>	t2 = insert 5 t1
>	t3 = insert 19 t2
>	t4 = insert 7 t3

>	v1 = find 19 t1
>	v2 = find 19 t3


BSTTree3 sanity check

*Main> t4
Branch 5 Nil (Branch 19 (Leaf 7) Nil)
*Main> v1
False
*Main> v2
True
*Main>
*Main> delete 19 t4
Branch 5 Nil (Branch 7 Nil Nil)


BSTTree2 sanity check

*Main> t4
N (5,Nil,N (19,N (7,Nil,Nil),Nil))
*Main> v1
False
*Main> v2
True
*Main> delete 19 t4
N (5,Nil,N (7,Nil,Nil))


BSTTree sanity check

*Main> t4
Node 5 Empty (Node 19 (Node 7 Empty Empty) Empty)
*Main> v1
False
*Main> v2
True
*Main> delete 19 t4
Node 5 Empty (Node 7 Empty Empty)



