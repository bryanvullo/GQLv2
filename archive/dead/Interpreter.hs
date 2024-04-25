module Interpreter 
    (interpret) 
    where

import Parser 
import InputLexer
import InputParser 


interpret :: [Q] -> Tables
interpret statements = interpret' statements []
    -- case s of 
    --     X $ ClassFinalSet type varName command 
    --      -> do
    --         contents <- readFile file
    --         let newGraph = parseInput $ lexInput contents
    --         interpret statements newGraph

        -- interpret all the other statements
        {-
        FIND -> reduce the amount of rows in the graph to only the ones that match the query
            use a loop to go through each row and check if it matches the query
        OUT -> print the graph using Printer.hs
            Output must be a graph, ie Table or [Row] 
        CONDITIONS -> interpret the boolean expression, if true then interpret the statements
        LOOPF -> fetch the data we are looping and create a function here to loop over that data, 
            interpreting each row (node)
        Bool expressions -> trivial - use haskell's built in functions
        REMOVE -> remove the data from the graph, ie row from the table
        -}

interpret' :: [Q] -> Environment -> [Frame] -> Tables
interpret' (X (ClassFinalSet _ varName command) : statements) env frames = 
    interpret' (X command:statements) env (Assign varName env:frames)

interpret' (X (ACCESS file) : statements) env (Assign varName env':frames) =
    do
        contents <- readFile file
        let newGraph = parseInput $ lexInput contents
        interpret' statements ( (varName, newGraph) : env') frames

interpret' (X $ Set v1 v2 : statements) env frames = 
    interpret' 
 

type Environment = [(String, Data)]

data Frame = 
    Assign String Environment |
    Loop String

data Data = 
    G Tables |
    N [Row] | 
    B Bool |
    I Int | 
    S String 
