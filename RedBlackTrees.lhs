https://elmprogramming.com/

>	module RedBlackTrees where

import List

import Debug

>	import System.Random

>	data Color = R | B
>		deriving (Show, Eq)
>	data Tree  = E | T Color Tree Int Tree
>		deriving (Show, Eq)

>	empty = E

>	member :: Int -> Tree -> Bool
>	member x t@(T _ l y r) 
>		 | t == E	= False
>		 | x == y	= True
>		 | x < y		= member x l
>		 | x > y		= member x r

>	insert :: Int -> Tree -> Tree
>	insert x t = T B l y r
>		where
>			(T _ l y r) = ins x t
>			ins :: Int -> Tree -> Tree
>			ins x E = T R E x E
>			ins x u@(T c l y r)
>				| x == y	= u
>				| x < y		= balance c (ins x l) y r
>				| otherwise 	= balance c l y (ins x r)

>	balance :: Color -> Tree -> Int -> Tree -> Tree
>	balance c l val r = case (c, l, val, r) of
>	    (B, T R (T R a x b) y c, z, d) -> T R (T B a x b) y (T B c z d)
>	    (B, T R a x (T R b y c), z, d) -> T R (T B a x b) y (T B c z d)
>	    (B, a, x, T R (T R b y c) z d) -> T R (T B a x b) y (T B c z d)
>	    (B, a, x, T R b y (T R c z d)) -> T R (T B a x b) y (T B c z d)
>	    _                              -> T c l val r

--------------------------------------------------------------------------

>	color t = case t of {T c _ _ _ -> c; E -> B}
>	root  t = case t of {T _ _ x _ -> x}
>	left  t = case t of {T _ l _ _ -> l}
>	right t = case t of {T _ _ _ r -> r}

>	height t = case t of
>	  E         -> 0
>	  T _ l _ r -> 1 + max (height l) (height r)

>	size t = case t of
>	  E         -> 0
>	  T _ l _ r -> 1 + size l + size r

>	bso t = case t of
>	  E         -> True
>	  T _ l x r -> (l == E || root l < x) &&
>	               (r == E || x < root r) &&
>	               bso l && bso r

>	blackHeight t = case t of
>	  E -> Just 0
>	  T c l _ r ->
>	    let i = case c of {B -> 1; R -> 0} in
>	    case (blackHeight l, blackHeight r) of
>	      (Just n, Just m) -> if
>	        (n == m) then Just (i + n)
>	        else  Nothing
>	      _ -> Nothing

>	okBlack t = case blackHeight t of {Just _ -> True; Nothing -> False}

>	bh t = fromJust $ blackHeight t

>	fromJust mx = case mx of Just x -> x

>	noRedRed t = case t of
>	  E                   -> True
>	  T R (T R _ _ _) _ _ -> False
>	  T R _ _ (T R _ _ _) -> False
>	  T _ l _ r           -> and $ map noRedRed [l, r]

>	oneRedRed t = case t of
>	  E                             -> False
>	  T R (T R _ _ _) _ (T R _ _ _) -> False
>	  T R (T R l1 _ r1) _ r         -> and $ map noRedRed [l1, r1, r]
>	  T R l _ (T R l2 _ r2)         -> and $ map noRedRed [l, l2, r2]
>	  T _ l _ r                     -> False

>	maybeOneRedRed t = oneRedRed t || noRedRed t

>	rb t = bso t && color t == B && noRedRed t && okBlack t

>	rbExceptRoot t = bso t && noRedRed t && okBlack t

>	rbPlus t = rb t &&
>	           (height t <= 2 * (1 + logBase 2 (size t)))

>	xor bs = filter (\b -> b == True) bs == [True]

------------------------------------------------------------------------------

>	check s p x =
>	  if  (p x)  then x
>	     else error (s ++ "\n" ++ show x)

>	checkArg s = check ("ARG CONTRACT ERROR: " ++ s)
>	checkRet s = check ("RET CONTRACT ERROR: " ++ s)

-- insert' : x:Int -> t:Tree{rb t} -> t':Tree{rb t'}

>	insert' x t_ = checkRet "[insert']" rb ret
>		where
>			t = checkArg "[insert']" rb t_
>			(T _ l y r) = ins' x t
>			ret = T B l y r

-- ins' : x:Int -> t:Tree{rb t} -> t':Tree{bh t' == bh t && maybeOneRedRed t'}

>	ins' x t_ = checkRet "[ins']" (\t' -> bh t' == bh t
>		&& maybeOneRedRed t') ret
>		where
>	  		t = checkArg "[ins']" rbExceptRoot t_ 
>	  		ret =  case t of
>	      			E -> T R E x E
>	      			T c l y r -> if (x == y) then t
>	        		else if ( x <  y)
>					then balance' c (ins' x l) y r
>	        		else balance' c l y (ins' x r)

-- balance' : c:Color
--         -> l:Tree
--         -> val:Int
--         -> r:Tree{bh l == bh r && xor [ ... ]}
--         -> t':Tree{bh t' == bh (T c l val r) && maybeOneRedRed t'}

>	balance' c l val r_ =
>	  checkRet "[balance']"
>	    (\t' -> maybeOneRedRed t' && bh t' == bh (T c l val r)) ret
>	  where
>	  	r =  checkArg "[balance']" (\arg ->
>	      		(bh l == bh arg) &&
>	      		(xor [  noRedRed l &&  noRedRed arg
>	           		, oneRedRed l &&  noRedRed arg
>	           		,  noRedRed l && oneRedRed arg ])) r_
>	  	ret =  case (c, l, val, r) of
>	      		(B, T R (T R a x b) y c, z, d) -> T R (T B a x b) y (T B c z d)
>	      		(B, T R a x (T R b y c), z, d) -> T R (T B a x b) y (T B c z d)
>	      		(B, a, x, T R (T R b y c) z d) -> T R (T B a x b) y (T B c z d)
>	      		(B, a, x, T R b y (T R c z d)) -> T R (T B a x b) y (T B c z d)
>	      		_                              -> T c l val r

------------------------------------------------------------------------------

>	randomInts n =
>	  let seed = mkStdGen 0 in
>	  randomRs (0, 10000) seed


>	build fInsert ns =
>	     checkRet "[build]" rbPlus $ foldl fInsert empty ns

>	buildRandom fInsert n = build fInsert (randomInts n)

-- NOTE: working around bug in REPL

	le = (<=)

