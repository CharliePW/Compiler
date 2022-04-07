module Interpreter
(
    AExp(..),
    BExp(..),
    Com (..),
    aval,
    bval,
    eval
) where

import Data.Map
import Machine

--TODO Task 2.1
data AExp =
    N Val
    | V Vname
    | Plus AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.2
aval :: AExp -> State -> Val
aval (N n) stt = n 
aval (V x) stt = stt ! x 
aval (Plus a1 a2) stt = aval a1 stt + aval a2 stt

--TODO Task 2.1
data BExp =
    Bc Bool
    | Not BExp
    | And BExp BExp
    | Less AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.3
bval :: BExp -> State -> Bool
bval (Bc v) stt = v 
bval (Not b) stt = not (bval b stt)
bval (And b1 b2) stt = bval b1 stt && bval b2 stt
bval (Less a1 a2) stt = aval a1 stt < aval a2 stt


--TODO Task 2.1
data Com = 
    SKIP
    | Assign Vname AExp
    | Seq Com Com
    | If BExp Com Com
    | While BExp Com
    deriving (Eq, Read, Show)

--TODO Task 2.4
eval :: Com -> State -> State
eval (SKIP) stt = stt
eval (Assign x n) stt = insert x (aval n stt) stt
eval (Seq c1 c2) stt = eval c2 (eval c1 stt) 
eval (If b c1 c2) stt 
    | bval b stt = eval c1 stt
    | otherwise = eval c2 stt
eval (While b c) stt = 
    if bval b stt 
        then eval (While b c) (eval c stt)
    else eval SKIP stt 

