> import Data.List

> type Symbol = String

> type Tape = ([Symbol],[Symbol])

> type Transition = [(Symbol,Symbol,Symbol,Symbol,Symbol)]

> type OutputTape = [Symbol]

> type NextState = Symbol

> type NextSymbol = Symbol

> type Direction = Symbol


> eval1 :: Transition -> Tape -> (Tape,Maybe Symbol)
> eval1 [] t = (t, Nothing)
> eval1 ((src,symbol,dest,newSym,direction):trans) t@(prvT,(currSym:restSym)) = if symbol == currSym then (newTape,Just nextState) else eval1 trans t
>                                                                               where (newTape,nextState) = doTrans newSym dest direction t     

Take nextsymbol, dest, and direction and return nextstate with modifed tape

> doTrans :: NextSymbol -> NextState -> Direction -> Tape -> (Tape,Symbol)
> doTrans nextSym nextSt dir (prvT,(x:xs)) = case dir of 
>                                           "L"   -> if prvT /= [] then ( ( (prvT \\ [last prvT]),((last prvT):nextSym:xs) ), nextSt) else (([nextSym],xs),nextSt)
>                                           "R"   -> ( ( prvT ++ [nextSym], xs) , nextSt)