> import Data.List

> type Symbol = String

> type Tape = ([Symbol],[Symbol])

> type Transition = [(Symbol,Symbol,Symbol,Symbol,Symbol)]

> type OutputTape = [Symbol]

> type NextState = Symbol

> type NextSymbol = Symbol

> type Direction = Symbol


> eval1 :: Transition -> Tape -> Symbol -> (Tape,Maybe Symbol)
> eval1 [] t _ = (t, Nothing)
> eval1 ((src,symbol,dest,newSym,direction):trans) t@(prvT,(currSym:restSym)) s = if symbol == currSym && src == s then (newTape,Just nextState) else eval1 trans t s
>                                                                               where (newTape,nextState) = doTrans newSym dest direction t     

Take nextsymbol, dest, and direction and return nextstate with modifed tape

> doTrans :: NextSymbol -> NextState -> Direction -> Tape -> (Tape,Symbol)
> doTrans nextSym nextSt dir (prvT,(x:xs)) = case dir of 
>                                           "L"   -> if prvT /= [] then  ( (init prvT,((last prvT):nextSym:xs) ), nextSt) else error "Going beyond beginning of tape"
>                                           "R"   -> ( ( prvT ++ [nextSym], xs) , nextSt)

eval function that returns tape once no transitions apply

> eval :: Transition -> Tape -> Symbol -> Tape
> eval tran tape state = case eval1 tran tape state of
>                        (tape2,Nothing)       -> tape2
>                        (tape2,Just state2)   -> eval tran tape2 state2

Example 8.1.1 machine

> m =  [("q0","B","q1","B","R"), ("q1","a","q1","b","R"), ("q1","b","q1","a","R"), ("q1","B","q2","B","L"), ("q2","a","q2","a","L"), ("q2","b","q2","b","L")]

> input = ([],["B","a","b","a","b","B"])

*Main> eval m input "q0"
([],["B","b","a","b","a","B"])
(0.01 secs, 120,960 bytes)
