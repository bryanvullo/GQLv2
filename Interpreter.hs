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
