st in Elm has basic List functions which we already
have defined. See https://package.elm-lang.org/packages/elm/core/latest/List
And here is the original Elm file for ListsAndTrees:
https://www.classes.cs.uchicago.edu/archive/2015/winter/22300-1/public-code/hw2/ListsAndTrees.elm

>	module ListsAndTrees where

In Elm you have to import List, and :: is the List cons operator
in Elm. It's : in Haskell.

	import List ((::))
	import List

>	suffixes xs =
>	  []

In Elm, the data type is type, in Haskell it's data

>	data Tree = Empty | Node Int Tree Tree deriving(Show, Eq)

In Elm, the type indicator is :, in Haskell it's ::

>	mem :: Int -> Tree -> Bool
>	mem _ _ =
>	  False

>	fullTree :: Int -> Int -> Tree
>	fullTree _ _ =
>	  Empty

>	balancedTree :: Int -> Int -> Tree
>	balancedTree _ _ =
>	  Empty

>	create2 :: Int -> Int -> (Tree, Tree)
>	create2 _ _ =
>	  (Empty, Empty)

In Elm we use List Type to indicate a list of type Type.
In Haskell, just [Type]

>	balancedTrees :: Int -> Int -> [Tree]
>	balancedTrees _ _ =
>	  []

>	completeTrees :: Int -> Int -> [Tree]
>	completeTrees _ _ =
>	  []

>	almostCompleteTrees :: Int -> Int -> [Tree]
>	almostCompleteTrees _ _ =
>	  []

