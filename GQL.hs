import System.Environment (getArgs)
import Parser
import Lexer
import InputParser (parseInput, Tables, Table, Row, ID, Labels)
import InputLexer (lexInput, Token(..))
import Interpreter (interpret)
import Printer (printOutput)

main :: IO ()
main = do
    (filename:_) <- getArgs
    contents <- readFile filename
    let tokens = alexScanTokens contents
    let result = parser tokens

    --comment these out for Submission
    print result 
    putStrLn ""

    -- test input parser
    -- let inputResult = parseInput $ lexInput contents
    -- print inputResult

    -- interpret the query
    interpret result