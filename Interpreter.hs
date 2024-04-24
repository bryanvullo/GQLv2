module Interpreter (interpret) where

import InputParser (Tables, Row(..), ID(..), Value(..), Labels, Relationship)
import Parser (QQ, Q(..), X(..), NumericXX(..), BoolXX(..), Class(..))
import Data.List (nub, intersect)
import Data.Maybe (fromJust, isJust)

interpret :: QQ -> Tables -> Tables
interpret qq tables = interpretQQ qq tables

interpretQQ :: QQ -> Tables -> Tables
interpretQQ [] tables = tables
interpretQQ (q:qs) tables = 
    let updatedTables = interpretQ q tables
    in interpretQQ qs updatedTables

interpretQ :: Q -> Tables -> Tables
interpretQ (X x) tables = [interpretX x tables]
interpretQ (CONDIFQ cond ifQQ) tables =
    if interpretBoolXX cond tables
        then interpretQQ ifQQ tables
        else tables
interpretQ (CONDELIFQ cond ifQQ elseQQ) tables =
    if interpretBoolXX cond tables
        then interpretQQ ifQQ tables
        else interpretQQ elseQQ tables
interpretQ (THROUGHQ cls label x qq) tables =
    concatMap (\table -> interpretQQ qq [interpretX (Set (Identifier label) (toX table)) tables]) $
        filterTables (\table -> interpretClass cls table) tables
    where
        toX :: [Row] -> X
        toX rows = ClassFinalSet Graph label (ACCESS "temp.n4j")

        filterTables :: ([Row] -> Bool) -> Tables -> Tables
        filterTables predicate = filter predicate

interpretX :: X -> Tables -> [Row]
interpretX (ClassFinalSet cls label x) tables = 
    let result = interpretX x tables
    in [Header [interpretClass cls result]]
interpretX (Set x1 x2) tables =
    let result1 = interpretX x1 tables
        result2 = interpretX x2 tables
    in updateTable result1 result2
interpretX (ClassShow cls label) tables = [Header [interpretClass cls []]]
interpretX (Identifier label) tables = findTable label tables
interpretX (NumericXX numXX) tables = [interpretNumericXX numXX tables]
interpretX (Chars str) tables = [Data (Id str) []]
interpretX (Regular regex) tables = filterTable (\row -> isMatchingRow row regex) (concat tables)
interpretX (CASEQ x boolXX) tables =
    let result = interpretX x tables
        predicate row = interpretBoolXX boolXX [row]
    in filterTable predicate result
interpretX (PlusQ x1 x2) tables =
    let result1 = interpretX x1 tables
        result2 = interpretX x2 tables
    in result1 ++ result2
interpretX (ACCESS fileName) tables = parseInput fileName
interpretX (STDOUT label) tables = 
    let result = findTable label tables
    in result -- Output the result
interpretX (BoolXX boolXX) tables = [BoolValue (interpretBoolXX boolXX tables)]
interpretX (CallAttribute x attr) tables =
    let result = interpretX x tables
        attrValue = getAttribute attr result
    in [attrValue]
interpretX (CallAssociation x boolXX) tables =
    let result = interpretX x tables
        predicate row = interpretBoolXX boolXX [row]
        relatedRows = filterTable predicate (concat tables)
    in getRelatedRows result relatedRows
interpretX (NumericIncrease x1 x2) tables =
    let result1 = interpretX x1 tables
        result2 = interpretX x2 tables
        increaseValue = getNumericValue result2
    in numericIncrease result1 increaseValue
interpretX (NumericDecrease x1 x2) tables =
    let result1 = interpretX x1 tables
        result2 = interpretX x2 tables
        decreaseValue = getNumericValue result2
    in numericDecrease result1 decreaseValue
interpretX (Not x1 x2) tables =
    let result1 = interpretX x1 tables
        result2 = interpretX x2 tables
    in filterTable (\row -> not (elem row result2)) result1
interpretX (CallDataPoint x boolXX) tables =
    let result = interpretX x tables
        predicate row = interpretBoolXX boolXX [row]
        dataPoints = filterTable predicate (concat tables)
    in getDataPoints result dataPoints

interpretNumericXX :: NumericXX -> Tables -> Row
interpretNumericXX (NumericTerminal n) _ = IntValue n
interpretNumericXX (PlusPlus n1 n2) tables =
    let IntValue v1 = interpretNumericXX n1 tables
        IntValue v2 = interpretNumericXX n2 tables
    in IntValue (v1 + v2)
interpretNumericXX (NumericSubtract n1 n2) tables =
    let IntValue v1 = interpretNumericXX n1 tables
        IntValue v2 = interpretNumericXX n2 tables
    in IntValue (v1 - v2)
interpretNumericXX (NumericMultiply n1 n2) tables =
    let IntValue v1 = interpretNumericXX n1 tables
        IntValue v2 = interpretNumericXX n2 tables
    in IntValue (v1 * v2)
interpretNumericXX (NumericDivide n1 n2) tables =
    let IntValue v1 = interpretNumericXX n1 tables
        IntValue v2 = interpretNumericXX n2 tables
    in IntValue (v1 `div` v2)

interpretBoolXX :: BoolXX -> Tables -> Bool
interpretBoolXX (BoolTerminal b) _ = b
interpretBoolXX (Identical x1 x2) tables =
    let result1 = interpretX x1 tables
        result2 = interpretX x2 tables
    in result1 == result2
interpretBoolXX (IdenticalNot x1 x2) tables =
    let result1 = interpretX x1 tables
        result2 = interpretX x2 tables
    in result1 /= result2
interpretBoolXX (InequalityStrictLesser x1 x2) tables =
    let IntValue v1 = head (interpretX x1 tables)
        IntValue v2 = head (interpretX x2 tables)
    in v1 < v2
interpretBoolXX (InequalityStrictGreater x1 x2) tables =
    let IntValue v1 = head (interpretX x1 tables)
        IntValue v2 = head (interpretX x2 tables)
    in v1 > v2
interpretBoolXX (InequalitySlackLesser x1 x2) tables =
    let IntValue v1 = head (interpretX x1 tables)
        IntValue v2 = head (interpretX x2 tables)
    in v1 <= v2
interpretBoolXX (InequalitySlackGreater x1 x2) tables =
    let IntValue v1 = head (interpretX x1 tables)
        IntValue v2 = head (interpretX x2 tables)
    in v1 >= v2
interpretBoolXX (LogicalAnd b1 b2) tables =
    interpretBoolXX b1 tables && interpretBoolXX b2 tables
interpretBoolXX (LogicalOr b1 b2) tables =
    interpretBoolXX b1 tables || interpretBoolXX b2 tables
interpretBoolXX (LogicalNegate b) tables = not (interpretBoolXX b tables)
interpretBoolXX (AssociationEndQ boolXX label) tables =
    let predicate row = interpretBoolXX boolXX [row]
        relatedRows = filterTable predicate (concat tables)
        endIds = map (\(RelationshipData _ _ endId _) -> endId) relatedRows
    in elem (Id label) endIds
interpretBoolXX (AssociationStartQ startLabel boolXX) tables =
    let predicate row = interpretBoolXX boolXX [row]
        relatedRows = filterTable predicate (concat tables)
        startIds = map (\(RelationshipData startId _ _ _) -> startId) relatedRows
    in elem (Id startLabel) startIds
interpretBoolXX (AssociationQ startLabel boolXX endLabel) tables =
    let predicate row = interpretBoolXX boolXX [row]
        relatedRows = filterTable predicate (concat tables)
        associations = map (\(RelationshipData startId _ endId _) -> (startId, endId)) relatedRows
    in elem (Id startLabel, Id endLabel) associations
interpretBoolXX (AssociationNodeLabelQ startLabel boolXX endLabels) tables =
    let predicate row = interpretBoolXX boolXX [row]
        relatedRows = filterTable predicate (concat tables)
        associations = map (\(RelationshipData startId _ _ _) -> startId) relatedRows
        endNodes = filterTable (\(LabeledData _ _ labels) -> any (`elem` endLabels) labels) (concat tables)
        endIds = map (\(LabeledData endId _ _) -> endId) endNodes
    in elem (Id startLabel) associations && any (`elem` endIds) associations
interpretBoolXX (Has x labels) tables =
    let result = interpretX x tables
        hasAll = all (\label -> isJust (getAttribute label result)) labels
    in hasAll

interpretClass :: Class -> [Row] -> Row
interpretClass Graph rows = Header (getTypes rows)
interpretClass Num _ = IntType "numeric"
interpretClass String _ = StringType "string"
interpretClass Bool _ = BoolType "boolean"
interpretClass Node rows = Header (getNodeTypes rows)
interpretClass Edge rows = RelationshipHeader (getEdgeTypes rows)

findTable :: String -> Tables -> [Row]
findTable label tables = fromJust (lookup (Id label) (zip (map getId tables) tables))
    where
        getId :: [Row] -> ID
        getId ((Data id _):_) = id
        getId ((LabeledData id _ _):_) = id
        getId ((RelationshipData startId _ _ _):_) = startId
        getId _ = error "Invalid table format"

updateTable :: [Row] -> [Row] -> [Row]
updateTable _ [] = []
updateTable [] _ = []
updateTable ((Data id values):rows) ((Data _ newValues):newRows) = 
    Data id (updateValues values newValues) : updateTable rows newRows
updateTable ((LabeledData id values labels):rows) ((LabeledData _ newValues newLabels):newRows) =
    LabeledData id (updateValues values newValues) (labels ++ newLabels) : updateTable rows newRows
updateTable ((RelationshipData startId values endId rel):rows) ((RelationshipData _ newValues _ _):newRows) =
    RelationshipData startId (updateValues values newValues) endId rel : updateTable rows newRows
updateTable _ _ = error "Invalid table format for update"

updateValues :: [Value] -> [Value] -> [Value]
updateValues [] newValues = newValues
updateValues values [] = values
updateValues (v:vs) (newV:newVs) =
    case (v, newV) of
        (NullValue, _) -> newV : updateValues vs newVs
        (_, NullValue) -> v : updateValues vs newVs
        _ -> newV : updateValues vs newVs

getAttribute :: String -> [Row] -> Value
getAttribute attr ((Data _ values):_) = getValue attr values
getAttribute attr ((LabeledData _ values _):_) = getValue attr values
getAttribute attr ((RelationshipData _ values _ _):_) = getValue attr values
getAttribute _ _ = NullValue

getValue :: String -> [Value] -> Value
getValue _ [] = NullValue
getValue attr (v@(StringValue x):vs) 
    | attr == x = v
    | otherwise = getValue attr vs
getValue attr (v@(IntValue x):vs)
    | attr == show x = v
    | otherwise = getValue attr vs
getValue attr (v@(BoolValue x):vs)
    | attr == show x = v
    | otherwise = getValue attr vs
getValue attr (_:vs) = getValue attr vs

filterTable :: (Row -> Bool) -> [Row] -> [Row]
filterTable predicate = filter predicate

isMatchingRow :: Row -> String -> Bool
isMatchingRow (Data id _) regex = isMatching (idToString id) regex
isMatchingRow (LabeledData id _ labels) regex = 
    isMatching (idToString id) regex || any (\label -> isMatching label regex) labels
isMatchingRow (RelationshipData startId _ endId rel) regex =
    isMatching (idToString startId) regex ||
    isMatching (idToString endId) regex ||
    isMatching rel regex
isMatchingRow _ _ = False

isMatching :: String -> String -> Bool
isMatching str regex = str =~ regex

idToString :: ID -> String
idToString (Id str) = str

numericIncrease :: [Row] -> Int -> [Row]
numericIncrease rows increaseValue = map (incrementNumericValues increaseValue) rows

numericDecrease :: [Row] -> Int -> [Row]
numericDecrease rows decreaseValue = map (decrementNumericValues decreaseValue) rows

incrementNumericValues :: Int -> Row -> Row
incrementNumericValues _ (Header types) = Header types
incrementNumericValues inc (Data id values) = Data id (map (incrementValue inc) values)
incrementNumericValues inc (LabeledData id values labels) = LabeledData id (map (incrementValue inc) values) labels
incrementNumericValues inc (RelationshipData startId values endId rel) = 
    RelationshipData startId (map (incrementValue inc) values) endId rel

decrementNumericValues :: Int -> Row -> Row
decrementNumericValues _ (Header types) = Header types
decrementNumericValues dec (Data id values) = Data id (map (decrementValue dec) values)
decrementNumericValues dec (LabeledData id values labels) = LabeledData id (map (decrementValue dec) values) labels
decrementNumericValues dec (RelationshipData startId values endId rel) = 
    RelationshipData startId (map (decrementValue dec) values) endId rel

incrementValue :: Int -> Value -> Value
incrementValue _ NullValue = NullValue
incrementValue inc (IntValue x) = IntValue (x + inc)
incrementValue _ v = v

decrementValue :: Int -> Value -> Value
decrementValue _ NullValue = NullValue
decrementValue dec (IntValue x) = IntValue (x - dec)
decrementValue _ v = v

getNumericValue :: [Row] -> Int
getNumericValue ((IntValue x):_) = x
getNumericValue _ = 0

getRelatedRows :: [Row] -> [Row] -> [Row]
getRelatedRows rows relatedRows = 
    let rowIds = map (\(Data id _) -> id) rows
        relatedRowIds = map (\(RelationshipData startId _ endId _) -> (startId, endId)) relatedRows
        filteredRelatedRows = filter (\(startId, endId) -> elem startId rowIds && elem endId rowIds) relatedRowIds
    in map (\(startId, endId) -> fromJust (find (\(RelationshipData sId _ eId _) -> sId == startId && eId == endId) relatedRows)) filteredRelatedRows

getDataPoints :: [Row] -> [Row] -> [Row]
getDataPoints rows dataPoints =
    let rowIds = map (\(Data id _) -> id) rows
        dataPointIds = map (\(Data id _) -> id) dataPoints
        commonIds = intersect rowIds dataPointIds
    in filter (\(Data id _) -> elem id commonIds) dataPoints

getTypes :: [Row] -> [Row]
getTypes rows = 
    let headers = filter (\r -> case r of { Header _ -> True; _ -> False }) rows
        in nub (concatMap (\(Header types) -> types) headers)

getNodeTypes :: [Row] -> [Row]
getNodeTypes rows =
   let dataRows = filter (\r -> case r of { Data _ _ -> True; LabeledData _ _ _ -> True; _ -> False }) rows
       dataTypes = nub (concatMap (\r -> case r of 
           Data _ values -> map (\v -> case v of
               StringValue _ -> StringType ""
               IntValue _ -> IntType ""
               BoolValue _ -> BoolType ""
               NullValue -> StringType "") values
           LabeledData _ values _ -> map (\v -> case v of
               StringValue _ -> StringType ""
               IntValue _ -> IntType ""
               BoolValue _ -> BoolType ""
               NullValue -> StringType "") values
           _ -> []) dataRows)
   in dataTypes

getEdgeTypes :: [Row] -> [Row]
getEdgeTypes rows =
   let relationshipRows = filter (\r -> case r of { RelationshipData _ _ _ _ -> True; _ -> False }) rows
       relationshipTypes = nub (concatMap (\(RelationshipData _ values _ _) -> map (\v -> case v of
           StringValue _ -> StringType ""
           IntValue _ -> IntType ""
           BoolValue _ -> BoolType ""
           NullValue -> StringType "") values) relationshipRows)
   in relationshipTypes

parseInput :: String -> Tables
parseInput fileName = 
   let inputStr = readFile fileName
       tokens = alexScanTokens inputStr
       parseResult = parseInput tokens
   in case parseResult of
       Left err -> error ("Parse error: " ++ show err)
       Right tables -> tables