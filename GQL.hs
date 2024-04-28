import System.Environment (getArgs)
import Parser
import Lexer
import InputParser (parseInput, Tables, Table, Row, ID, Labels)
import InputLexer (lexInput, Token(..))
-- import Interpreter (interpret)
import Printer (printOutput)

main :: IO ()
main = do
    (filename:_) <- getArgs
    contents <- readFile filename
    let tokens = alexScanTokens contents
    let result = parser tokens
    print result 
    

    -- Code graveyard

    -- let inputResult = parseInput $ lexInput contents
    -- print inputResult

    -- interpret the query
    -- output <- interpret result
    -- print output

    -- -- after we interpret we print the output
    -- printOutput output