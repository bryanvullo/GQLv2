import System.Environment (getArgs)
import Parser
import Lexer (alexScanTokens)
import InputParser (parseInput, Tables, Table, Row, ID, Labels)
import InputLexer (lexInput, Token(..))

main :: IO ()
main = do
    (filename:_) <- getArgs
    contents <- readFile filename
    -- let inputResult = parseInput $ lexInput contents
    -- print inputResult
    let tokens = alexScanTokens contents
    let result = parser tokens
    print result