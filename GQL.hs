import System.Environment (getArgs)
import Parser (parse)
import Lexer (Token)
import InputParser (parseInput, Tables, Table, Row, ID, Labels)
import InputLexer (lexInput, Token(..))
import Syntax (Query(..), PropertyRef, NodePattern, Condition, EdgeType, PropertyUpdate)

main :: IO ()
main = do
    (filename:_) <- getArgs
    contents <- readFile filename
    let result = parseInput $ lexInput contents
    print result