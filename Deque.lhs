>	{-# LANGUAGE  NamedFieldPuns #-}
>	{-# LANGUAGE  ExistentialQuantification  #-}
>	{-# LANGUAGE  StandaloneDeriving  #-}

Note that because I use == on the empty, I need to have the type var a
be an Eq type. Could alter this constraint.

>	module Deque (Deque, empty, isEmpty, addFront,
>		removeFront, peekFront, addBack, removeBack, peekBack) where

import List ((::))
import List

>	data Deque a =  (Show a, Eq a) => D { front :: [a], back :: [a] }
>	deriving instance Show (a) => Show (Deque a)
>	deriving instance Eq (a) => Eq (Deque a)

>	mkD :: (Show a, Eq a) => [a] -> [a] -> Deque a
>	mkD f b = D {front = f, back = b}

>	empty :: (Show a, Eq a) => Deque a
>	empty = mkD [] []

>	isEmpty :: (Show a, Eq a) => Deque a -> Bool
>	isEmpty q = q == empty

--------------------------------------------------------------------------------
-- FILL IN THE DEFINITIONS BELOW

>	check :: (Show a, Eq a) => [a] -> [a] -> Deque a
>	check f b | f == [] && b == [] = empty
>	          | f == [] && invariant b  = mkD ( reverse (drop (half b) b))  (take (half b) b)
>	          | b == [] && invariant b  = mkD (drop (half f) f) (reverse (take (half f) f))
>	          | True    = mkD f b
>	          where
>	            half = (\x -> (length x) `div` 2)
>	            invariant = (\x -> length (take 2 x) >= 2) 

	              
It creates a naming conflict when using front or back in a function soo
        
>	frnt = front
>	bck = back 

>	addFront :: (Show a, Eq a) => a -> Deque a -> Deque a
>	addFront x (D {front, back}) = case front of
>	                               [] -> mkD [x] back
>	                               _  -> mkD front (x:back)

>	addBack :: (Show a, Eq a) => a -> Deque a -> Deque a
>	addBack x (D {front, back}) = case back of
>	                              [] -> mkD front [x]
>	                              _ -> mkD front (x:back)

>	peekFront :: (Show a, Eq a) => Deque a -> Maybe a
>	peekFront (D {front, back}) = case front of
>	                              [x] -> Just x
>	                              []  -> Nothing

>	peekBack :: (Show a, Eq a) => Deque a -> Maybe a
>	peekBack (D {front, back}) =  case back of
>	                              [x] -> Just x
>	                              []  -> Nothing

Need to maintain variant that both front and back are nonempty if two elements are in queue

>	removeFront :: (Show a, Eq a) => Deque a -> Maybe (Deque a)
>	removeFront (D {front, back})    = case frnt newDQ of
>	                                   [] -> Nothing
>	                                   (x:t) -> Just (mkD t (bck newDQ)) 
>	                                   where 
>	                                   newDQ = check front back

>	removeBack :: (Show a, Eq a) => Deque a -> Maybe (Deque a)
>	removeBack (D {front, back}) = case bck newDQ of
>	                               [] -> Nothing
>	                               (x:t) -> Just (mkD (frnt newDQ) t)
>	                               where
>	                               newDQ = check front back

