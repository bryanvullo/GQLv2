module Interpreter (interpret) where

-- Importing necessary modules
import InputParser (parseInput, Tables, Table, Row(..), ID(..), Value(..), Labels, Relationship, Type(..))
import Parser (QQ, Q(..), X(..), NumericXX(..), BoolXX(..), Class(..))
import InputLexer (lexInput, Token(..))
import Data.List (isInfixOf)

-- Environment
type Env = [(String, Data)] -- Variable name, Data

-- Continuation Stack
data Kont =
    KEmpty -- Empty continuation
    | KAssign String Env Kont
    | KSetL X Env Kont
    | KSeq QQ Env Kont -- Sequence continuation
    | KCond BoolXX QQ QQ Env Kont -- Conditional continuation
    | KThrough Class String X QQ Env Kont -- Through continuation
    | KSet String X Env Kont -- Set continuation
    | KNumericXX NumericXX Env Kont -- Numeric continuation
    | KBoolXX BoolXX Env Kont -- Boolean continuation
    | KCallAttribute String Env Kont -- Call attribute continuation
    | KCallAssociation BoolXX Env Kont -- Call association continuation
    | KNumericIncrease X Env Kont -- Numeric increase continuation
    | KNumericDecrease X Env Kont -- Numeric decrease continuation
    | KDataPoint BoolXX Env Kont -- Data point continuation

-- Control State
type Control = (QQ, Env, Kont) -- Statements, Environment, Continuation

-- Data Type
data Data = G [Table] | N Row | B Bool | I Int | S String | Nil -- Graph, Node, Bool, Int, String, Nil

-- Interpreter Functions
interpret :: QQ -> Tables
interpret qq = interpretCEK (newQQ, [name, newGraph], KEmpty)
    where 
        --find access statement 
        (X ( ClassFinalSet _ name (ACCESS file))) = findAccess qq
        newGraph = parseInputFile file
        newQQ = filter (\x -> x /= X ( ClassFinalSet _ _ (ACCESS _))) qq

findAccess :: QQ -> Q 
findAccess qq = head $ filter (\x -> x == X ( ClassFinalSet _ _ (ACCESS _))) qq

interpretCEK :: Control -> Control
interpretCEK ([], env, KEmpty) = ([], env, KEmpty)
interpretCEK (s@(X (ClassFinalSet dType name x)):statements, env, kont) =
    interpretCEK (x:statements, env, KAssign name env kont)
interpretCEK (s@(X (Set x1 x2)):statements, env, kont) =
    interpretCEK (x1:statements, env, KSetL x2 env kont)
interpretCEK (s@(X (ClassShow dType name)):statements, env, kont) =
    interpretCEK (statements, (name, Nil):env, kont)
interpretCEK (s@(X (Identifier name)):statements, env, kont) =
    case lookup name env of
        Just value -> interpretCEK (statements, env, kont)
        Nothing -> error $ "Undefined variable: " ++ name
interpretCEK (s@(X (NumericXX numXX)):statements, env, kont) =
    interpretCEK (statements, env, KNumericXX numXX env kont)
interpretCEK (s@(X (Chars str)):statements, env, kont) =
    interpretCEK (statements, ("$str", S str):env, kont)
interpretCEK (s@(X (Regular regex)):statements, env, kont) =
    interpretCEK (statements, ("$regex", S regex):env, kont)
interpretCEK (s@(X (CASEQ x boolXX)):statements, env, kont) =
    interpretCEK (x:statements, env, KCase boolXX env kont)
interpretCEK (s@(X (PlusQ x1 x2)):statements, env, kont) =
    interpretCEK (x1:x2:statements, env, KPlus env kont)
interpretCEK (s@(X (ACCESS fileName)):statements, env, kont) =
    case parseInputFile fileName of
        Left err -> error err
        Right tables -> interpretCEK (statements, ("$tables", G tables):env, kont)
interpretCEK (s@(X (STDOUT name)):statements, env, kont) =
    case lookup name env of
        Just value -> do
            print value
            interpretCEK (statements, env, kont)
        Nothing -> error $ "Undefined variable: " ++ name
interpretCEK (s@(X (BoolXX boolXX)):statements, env, kont) =
    interpretCEK (statements, env, KBoolXX boolXX env kont)
interpretCEK (s@(X (CallAttribute x attr)):statements, env, kont) =
    interpretCEK (x:statements, env, KAttr attr env kont)
interpretCEK (s@(X (CallAssociation x boolXX)):statements, env, kont) =
    interpretCEK (x:statements, env, KAssoc boolXX env kont)
interpretCEK (s@(X (NumericIncrease x1 x2)):statements, env, kont) =
    interpretCEK (x1:x2:statements, env, KIncrease env kont)
interpretCEK (s@(X (NumericDecrease x1 x2)):statements, env, kont) =
    interpretCEK (x1:x2:statements, env, KDecrease env kont)
interpretCEK (s@(X (Not x1 x2)):statements, env, kont) =
    interpretCEK (x1:x2:statements, env, KNot env kont)
interpretCEK (s@(X (CallDataPoint x boolXX)):statements, env, kont) =
    interpretCEK (x:statements, env, KDataPoint boolXX env kont)
interpretCEK (s@(CONDIFQ boolXX qq):statements, env, kont) =
    interpretCEK (statements, env, KCondIf boolXX qq [] env kont)
interpretCEK (s@(CONDELIFQ boolXX qq1 qq2):statements, env, kont) =
    interpretCEK (statements, env, KCondElseIf boolXX qq1 qq2 env kont)
interpretCEK (s@(THROUGHQ cls label x qq):statements, env, kont) =
    interpretCEK (statements, env, KThrough cls label x qq env kont)

-- Continuation handlers
interpretCEK ([], env, KAssign name value k) =
    interpretCEK ([], (name, value):env, k)
interpretCEK (x:statements, env, KAssign name _ k) =
    interpretCEK (x:statements, env, KAssign name env k)
interpretCEK ([], env, KSetL x2 value k) =
    interpretCEK (x2:[], env, KAssign "$temp" value k)
interpretCEK (x1:statements, env, KSetL x2 _ k) =
    interpretCEK (x1:statements, env, KSetL x2 env k)
interpretCEK ([], env, KNumericXX numXX value k) =
    let result = interpretNumericXX numXX value
    in interpretCEK ([], ("$temp", I result):env, k)
interpretCEK (x:statements, env, KNumericXX numXX _ k) =
    interpretCEK (x:statements, env, KNumericXX numXX env k)
interpretCEK ([], env, KCase boolXX value k) =
    if interpretBoolXX boolXX value
        then interpretCEK ([], ("$temp", B True):env, k)
        else interpretCEK ([], ("$temp", B False):env, k)
interpretCEK (x:statements, env, KCase boolXX _ k) =
    interpretCEK (x:statements, env, KCase boolXX env k)
interpretCEK ([], env, KPlus value1 (G tables1) (KPlus value2 (G tables2) k)) =
    interpretCEK ([], ("$temp", G (tables1 ++ tables2)):env, k)
interpretCEK (x1:x2:statements, env, KPlus _ _ k) =
    interpretCEK (x1:x2:statements, env, KPlus env k)
interpretCEK ([], env, KBoolXX boolXX value k) =
    let result = interpretBoolXX boolXX value
    in interpretCEK ([], ("$temp", B result):env, k)
interpretCEK (x:statements, env, KBoolXX boolXX _ k) =
    interpretCEK (x:statements, env, KBoolXX boolXX env k)
interpretCEK ([], env, KAttr attr value k) =
    let result = getAttribute attr value
    in interpretCEK ([], ("$temp", result):env, k)
interpretCEK (x:statements, env, KAttr attr _ k) =
    interpretCEK (x:statements, env, KAttr attr env k)
interpretCEK ([], env, KAssoc boolXX value k) =
    let result = getRelatedRows value (interpretBoolXX boolXX value)
    in interpretCEK ([], ("$temp", G result):env, k)
interpretCEK (x:statements, env, KAssoc boolXX _ k) =
    interpretCEK (x:statements, env, KAssoc boolXX env k)
interpretCEK ([], env, KIncrease value1 (I n1) (KIncrease value2 (I n2) k)) =
    let result = numericIncrease value1 n2
    in interpretCEK ([], ("$temp", G result):env, k)
interpretCEK (x1:x2:statements, env, KIncrease _ _ k) =
    interpretCEK (x1:x2:statements, env, KIncrease env k)
interpretCEK ([], env, KDecrease value1 (I n1) (KDecrease value2 (I n2) k)) =
    let result = numericDecrease value1 n2
    in interpretCEK ([], ("$temp", G result):env, k)
interpretCEK (x1:x2:statements, env, KDecrease _ _ k) =
    interpretCEK (x1:x2:statements, env, KDecrease env k)
interpretCEK ([], env, KNot value1 (G tables1) (KNot value2 (G tables2) k)) =
    let result = filterTable (\row -> not (elem row tables2)) tables1
    in interpretCEK ([], ("$temp", G result):env, k)
interpretCEK (x1:x2:statements, env, KNot _ _ k) =
    interpretCEK (x1:x2:statements, env, KNot env k)
interpretCEK ([], env, KDataPoint boolXX value k) =
    let result = getDataPoints value (interpretBoolXX boolXX value)
    in interpretCEK ([], ("$temp", G result):env, k)
interpretCEK (x:statements, env, KDataPoint boolXX _ k) =
    interpretCEK (x:statements, env, KDataPoint boolXX env k)
interpretCEK ([], env, KCondIf boolXX qq _ _ k) =
    if interpretBoolXX boolXX (lookup "$temp" env)
        then interpretCEK (qq, env, k)
        else interpretCEK ([], env, k)
interpretCEK ([], env, KCondElseIf boolXX qq1 qq2 _ k) =
    if interpretBoolXX boolXX (lookup "$temp" env)
        then interpretCEK (qq1, env, k)
        else interpretCEK (qq2, env, k)
interpretCEK ([], env, KThrough cls label x qq _ k) =
    case lookup label env of
        Just (G tables) ->
            let results = map (\table -> interpretCEK (qq, (label, G table):env, k)) tables
            in interpretCEK ([], env, k)
        _ -> error $ "Invalid data for label: " ++ label

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

-- Parsing Function
parseInputFile :: String -> Tables
parseInputFile fileName = do
    contents <- readFile fileName
    let tokens = lexInput contents
    let result = parseInput tokens
    result