> import Data.Char

Takes an integer greater than or equal to zero
and places its digits left to right into a list

> digitsOfInt :: Int -> [Int]
> digitsOfInt n | n < 0 = []
>               | otherwise = [ digitToInt x | x <- (show n)]

Takes and returns the additive persistence

> addPers :: Int -> Int
> addPers n = additivePersistence digits 0
>             where digits = digitsOfInt n

> additivePersistence :: [Int] -> Int -> Int
> additivePersistence n c | length n  == 0 = 0
>                         | length n  == 1 = c
>                         | otherwise = additivePersistence sumDigits (c+1)
>                         where sumDigits = digitsOfInt $ sum n

> digRoot :: Int -> Int
> digRoot n | n < 0 = 0
>           | otherwise = digitalRoot n addPs 
>           where  addPs       = addPers n 

> digitalRoot n count     | count == 0 = n
>                         | otherwise = digitalRoot nSum newCount 
>                         where nSum = sum(digitsOfInt n) 
>                               newCount = count-1




> subSeq :: [Int] -> [[Int]]
> subSeq [] = [[]]
> subSeq (x:xs) = [x:ys | ys <- subSeq xs] ++ subSeq xs



