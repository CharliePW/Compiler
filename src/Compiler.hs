module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter

--TODO Task 3.1
acomp :: AExp -> [Instr]
acomp (N n) = [LOADI n]
acomp (V x) = [LOAD x]
acomp (Plus a1 a2) = acomp a1 ++ acomp a2 ++ [ADD] 


--TODO Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc v) f n = if v == f then [JMP n] else []
bcomp (Not b) f n = bcomp b (not f) n
bcomp (And b1 b2) f n = 
    let cb2 = bcomp b2 f n
        m = if f then length cb2 else length cb2 + n
        cb1 = bcomp b1 False m
    in cb1 ++ cb2 
bcomp (Less a1 a2) f n = head (acomp a1) : head (acomp a2) : if f then [JMPLESS n] else [JMPGE n]

--TODO Task 3.3
ccomp :: Com -> [Instr]
ccomp SKIP = []
ccomp (Assign x n) = acomp n ++ [STORE x]
ccomp (Seq c1 c2) = ccomp c1 ++ ccomp c2
ccomp (If b c1 c2) = 
    let cc1 = ccomp c1 
        cc2 = ccomp c2 
        cb = bcomp b False (length cc1 + 1)
    in cb ++ cc1 ++ JMP (length cc2) : cc2 
ccomp (While b c) =
    let cc = ccomp c 
        cb = bcomp b False (length cc + 1)
    in cb ++ cc ++ [JMP ( - (length cb + length cc + 1))]