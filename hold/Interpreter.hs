module Interpreter 
    (interpret) 
    where

import Parser 
import InputLexer
import InputParser 

-- interpret :: 
interpret (s : statements) varibales = 
    case s of 
        Access var file -> do
            contents <- readFile file
            let newGraph = parseInput $ lexInput contents
            interpret statements newGraph

        -- interpret all the other statements
