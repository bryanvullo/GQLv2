import System.Environment (getArgs)
import Parser (parser)
import Lexer (Token, lex)
import InputParser (parseInput, Tables, Table, Row, ID, Labels)
import InputLexer (lexInput, Token(..))
-- import Syntax (Query(..), PropertyRef, NodePattern, Condition, EdgeType, PropertyUpdate)

main :: IO ()
main = do
    (filename:_) <- getArgs
    contents <- readFile filename
    -- let inputResult = parseInput $ lexInput contents
    -- print inputResult
    let result = parser $ Lexer.lex contents
    print result  