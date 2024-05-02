import System.Environment (getArgs)
import Parser
import Lexer
import InputParser (parseInput, Tables, Table, Row, ID, Labels)
import InputLexer (lexInput, Token(..))
import Interpreter (interpret)

main :: IO ()
main = do
    (filename:_) <- getArgs
    contents <- readFile filename -- read the file
    let tokens = alexScanTokens contents -- lex the file
    let result = parser tokens -- parse the tokens

    -- interpret the query and format the output
    interpret result