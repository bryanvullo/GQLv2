module Interpreter (interpret) where

import InputParser (parseInput, Tables, Table, Row(..), ID(..), Value(..), Labels, Relationship, Type(..))
import Parser (QQ, Q(..), X(..), NumericXX(..), BoolXX(..), Class(..))
import InputLexer (lexInput, Token(..))
import Data.List (isInfixOf)

type Env = [(String, Data)]

data Kont =
    KEmpty
    | KSeq QQ Env Kont
    | KCond BoolXX QQ QQ Env Kont
    | KThrough Class String X QQ Env Kont
    | KSet String X Env Kont
    | KNumericXX NumericXX Env Kont
    | KBoolXX BoolXX Env Kont
    | KCallAttribute String Env Kont
    | KCallAssociation BoolXX Env Kont
    | KNumericIncrease X Env Kont
    | KNumericDecrease X Env Kont
    | KDataPoint BoolXX Env Kont

type Control = (QQ, Env, Kont)

data Data = G [Table] | N Row | B Bool | I Int | S String | Nil

interpret :: QQ -> Tables
interpret qq = interpretCEK (newQQ, [name, newGraph], KEmpty)
    where 
        --find access statement 
        (X ( ClassFinalSet _ name (ACCESS file))) = findAccess qq
        newGraph = parseInputFile file
        newQQ = filter (\x -> x /= X ( ClassFinalSet _ name (ACCESS file))) qq

findAccess :: QQ -> Q 
findAccess qq = head $ filter (\x -> x == X ( ClassFinalSet _ name (ACCESS file))) qq

interpretCEK :: Control -> Tables
interpretCEK (( s@(X _) : statements) , env, kont) = undefined

interpretQ :: Q -> Tables -> Tables
interpretQ q tables = undefined

interpretX :: X -> Tables -> [Row]
interpretX x tables = undefined

interpretNumericXX :: NumericXX -> Tables -> Value
interpretNumericXX numXX tables = undefined

interpretBoolXX :: BoolXX -> Tables -> Bool
interpretBoolXX boolXX tables = undefined

interpretClass :: Class -> [Row] -> Row
interpretClass cls rows = undefined

findTable :: String -> Tables -> [Row]
findTable label tables = undefined

updateTable :: [Row] -> [Row] -> [Row]
updateTable rows1 rows2 = undefined

updateValues :: [Value] -> [Value] -> [Value]
updateValues values1 values2 = undefined

getAttribute :: String -> [Row] -> Value
getAttribute attr rows = undefined

getValue :: String -> [Value] -> Value
getValue attr values = undefined

filterTable :: (Row -> Bool) -> [Row] -> [Row]
filterTable predicate rows = undefined

isMatchingRow :: Row -> String -> Bool
isMatchingRow row regex = undefined

isMatching :: String -> String -> Bool
isMatching str regex = isMatchingHelper str regex 0 0

isMatchingHelper :: String -> String -> Int -> Int -> Bool
isMatchingHelper str regex strIdx regexIdx
    | strIdx == length str && regexIdx == length regex = True
    | strIdx == length str || regexIdx == length regex = False
    | regex !! regexIdx == '.' = isMatchingHelper str regex (strIdx + 1) (regexIdx + 1)
    | regex !! regexIdx == '*' =
        isMatchingHelper str regex strIdx (regexIdx + 1) ||
        (strIdx < length str && isMatchingHelper str regex (strIdx + 1) regexIdx)
    | strIdx < length str && str !! strIdx == regex !! regexIdx =
        isMatchingHelper str regex (strIdx + 1) (regexIdx + 1)
    | otherwise = False

isSubstring :: String -> String -> Bool
isSubstring substr str = substr `isInfixOf` str

idToString :: ID -> String
idToString (Id str) = str

numericIncrease :: [Row] -> Int -> [Row]
numericIncrease rows value = undefined

numericDecrease :: [Row] -> Int -> [Row]
numericDecrease rows value = undefined

incrementNumericValues :: Int -> Row -> Row
incrementNumericValues value row = undefined

decrementNumericValues :: Int -> Row -> Row
decrementNumericValues value row = undefined

incrementValue :: Int -> Value -> Value
incrementValue value val = undefined

decrementValue :: Int -> Value -> Value
decrementValue value val = undefined

getNumericValue :: [Row] -> Int
getNumericValue rows = undefined

getRelatedRows :: [Row] -> [Row] -> [Row]
getRelatedRows rows1 rows2 = undefined

getDataPoints :: [Row] -> [Row] -> [Row]
getDataPoints rows1 rows2 = undefined

getTypes :: [Row] -> [Row]
getTypes rows = undefined

getNodeTypes :: [Row] -> [Row]
getNodeTypes rows = undefined

getEdgeTypes :: [Row] -> [Row]
getEdgeTypes rows = undefined

parseInputFile :: String -> Tables
parseInputFile fileName = do
    contents <- readFile fileName
    let tokens = lexInput contents
    let result = parseInput tokens
    result