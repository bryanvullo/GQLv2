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

-- findTable :: String -> Tables -> [Row]
-- findTable label tables = case lookup label (zip (map getTableLabel tables) tables) of
--     Just rows -> rows
--     Nothing -> []
--   where
--     getTableLabel :: Table -> String
--     getTableLabel ((Header _):_) = "Header"
--     getTableLabel ((LabeledHeader _):_) = "LabeledHeader"
--     getTableLabel ((RelationshipHeader _):_) = "RelationshipHeader"
--     getTableLabel ((Data (Id label) _):_) = label
--     getTableLabel ((LabeledData (Id label) _ _):_) = label
--     getTableLabel ((RelationshipData (Id label) _ _ _):_) = label

updateTable :: [Row] -> [Row] -> [Row]
updateTable rows1 rows2 = undefined

-- updateTable :: [Row] -> [Row] -> [Row]
-- updateTable [] rows2 = rows2
-- updateTable rows1 [] = rows1
-- updateTable (row1:rows1) (row2:rows2) = updatedRow : updateTable rows1 rows2
--   where
--     updatedRow = case (row1, row2) of
--         (Data id1 values1, Data _ values2) -> Data id1 (updateValues values1 values2)
--         (LabeledData id1 values1 labels1, LabeledData _ values2 labels2) ->
--             LabeledData id1 (updateValues values1 values2) (labels1 ++ labels2)
--         (RelationshipData id1 values1 id2 rel, RelationshipData _ values2 _ _) ->
--             RelationshipData id1 (updateValues values1 values2) id2 rel
--         _ -> row1

updateValues :: [Value] -> [Value] -> [Value]
updateValues values1 values2 = undefined

-- updateValues :: [Value] -> [Value] -> [Value]
-- updateValues [] values2 = values2
-- updateValues values1 [] = values1
-- updateValues (val1:vals1) (val2:vals2) = updatedValue : updateValues vals1 vals2
--   where
--     updatedValue = case (val1, val2) of
--         (NullValue, _) -> val2
--         (_, NullValue) -> val1
--         _ -> val2

getAttribute :: String -> [Row] -> Value
getAttribute attr rows = undefined

-- getAttribute :: String -> [Row] -> Value
-- getAttribute _ [] = NullValue
-- getAttribute attr (row:rows) = case row of
--     Data _ values -> getValue attr values
--     LabeledData _ values _ -> getValue attr values
--     RelationshipData _ values _ _ -> getValue attr values
--     _ -> getAttribute attr rows

getValue :: String -> [Value] -> Value
getValue attr values = undefined

-- getValue :: String -> [Value] -> Value
-- getValue _ [] = NullValue
-- getValue attr (value:values) = case value of
--     StringValue str | attr == str -> value
--     IntValue i | attr == show i -> value
--     BoolValue b | attr == show b -> value
--     _ -> getValue attr values

filterTable :: (Row -> Bool) -> [Row] -> [Row]
filterTable predicate rows = undefined

-- filterTable :: (Row -> Bool) -> [Row] -> [Row]
-- filterTable predicate = filter predicate

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

-- numericIncrease :: [Row] -> Int -> [Row]
-- numericIncrease = map . incrementNumericValues

numericDecrease :: [Row] -> Int -> [Row]
numericDecrease rows value = undefined

-- numericDecrease :: [Row] -> Int -> [Row]
-- numericDecrease = map . decrementNumericValues

incrementNumericValues :: Int -> Row -> Row
incrementNumericValues value row = undefined

-- incrementNumericValues :: Int -> Row -> Row
-- incrementNumericValues value row = case row of
--     Data id values -> Data id (map (incrementValue value) values)
--     LabeledData id values labels -> LabeledData id (map (incrementValue value) values) labels
--     RelationshipData id1 values id2 rel -> RelationshipData id1 (map (incrementValue value) values) id2 rel
--     _ -> row

decrementNumericValues :: Int -> Row -> Row
decrementNumericValues value row = undefined

-- decrementNumericValues :: Int -> Row -> Row
-- decrementNumericValues value row = case row of
--     Data id values -> Data id (map (decrementValue value) values)
--     LabeledData id values labels -> LabeledData id (map (decrementValue value) values) labels
--     RelationshipData id1 values id2 rel -> RelationshipData id1 (map (decrementValue value) values) id2 rel
--     _ -> row

incrementValue :: Int -> Value -> Value
incrementValue value val = undefined

-- incrementValue :: Int -> Value -> Value
-- incrementValue value val = case val of
--     IntValue i -> IntValue (i + value)
--     _ -> val

decrementValue :: Int -> Value -> Value
decrementValue value val = undefined

-- decrementValue :: Int -> Value -> Value
-- decrementValue value val = case val of
--     IntValue i -> IntValue (i - value)
--     _ -> val

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