module Main where

import System.Environment
import Compiler
import Interpreter

--TODO Task 3.4
main :: IO ()
main = do
    input <- getArgs
    let command = read (head input) :: Com 
    print(ccomp command)