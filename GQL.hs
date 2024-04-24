import System.Environment (getArgs)
import Parser
import Lexer (alexScanTokens)
import InputParser (parseInput, Tables, Table, Row, ID, Labels)
import InputLexer (lexInput, Token(..))
import Interpreter (interpret)
import Printer (printOutput)

main :: IO ()
main = do
    (filename:_) <- getArgs
    contents <- readFile filename

    let result = parser $ Lexer.lex contents
    print result  
    
    -- interpret the query
    -- let output = interpret result
    -- print output

    -- after we interpret we print the output
    -- let result = parseInput $ lexInput contents
    -- printOutput result