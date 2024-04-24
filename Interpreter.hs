module Interpreter (interpret) where

import InputParser (Tables, Row(..), ID(..), Value(..), Labels, Relationship, Type(..))
import Parser (QQ, Q(..), X(..), NumericXX(..), BoolXX(..), Class(..))
import InputLexer (lexInput, Token(..))

data Env = Env {
    tables :: Tables,
    variables :: [(String, X)]
}

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

type Control = (X, Env, Kont)

interpret :: QQ -> Tables -> Tables
interpret qq tables = undefined

interpretCEK :: Control -> Tables
interpretCEK control = undefined

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
isMatching str regex = undefined

idToString :: ID -> String
idToString id = undefined

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