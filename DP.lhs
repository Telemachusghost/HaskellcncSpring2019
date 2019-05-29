-- This version uses the "array" library.

> import Data.Array((!), listArray)

> buyable n = r!n
>             where
>             r = listArray (0,n) (map f [0..n])
>             f i = i == 0 || i >= 6 && r!(i-6) || i >= 9 && r!(i-9) || i >= 20 && r!(i-20)