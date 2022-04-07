module Machine
(      
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where

import Data.Map ( (!), insert, Map )

--TODO Task 1.1
type Vname = String
--TODO Task 1.2
type Val = Int
--TODO Task 1.3
type State = Map Vname Val

--TODO Task 1.4
data Instr =
        LOADI Val
        | LOAD Vname
        | ADD 
        | STORE Vname
        | JMP Val
        | JMPLESS Val
        | JMPGE Val
        deriving (Eq, Read, Show)

--TODO Task 1.5
type Stack = [Val]

--TODO Task 1.6
type Config = (Int, State, Stack)

--TODO Task 1.7
iexec :: Instr -> Config -> Config
iexec (LOADI x) (i,stt,stk) = (succ i, stt, x : stk)
iexec (LOAD v) (i,stt,stk) = (succ i, stt,  stt ! v : stk)
iexec ADD (i,stt,stk) = (succ i, stt, head stk + head tl : tail tl)
        where 
        tl = tail stk
iexec (STORE v) (i,stt,stk) = (succ i, insert v (head stk) stt, tail stk) 
iexec (JMP n) (i,stt,stk) = (succ i + n, stt, stk)
iexec (JMPLESS n) (i,stt,stk) 
        | y < x = (succ i + n,stt, tail tl)
        | otherwise = (succ i,stt,tail tl)
        where 
        tl = tail stk 
        x = head stk 
        y = head tl
iexec (JMPGE n) (i,stt,stk) 
        | y >= x = (succ i + n,stt, tail tl)
        | otherwise = (succ i,stt,tail tl)
        where 
        tl = tail stk 
        x = head stk 
        y = head tl

--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec [] (i,stt,stk) = (i,stt,stk)
exec (x:xs) (i,stt,stk) = exec xs (iexec x (i,stt,stk))
