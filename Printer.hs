-- module Printer (printOutput, printTables, printRow, groupNodesToTables, GraphValue(..)) where

-- import InputParser
-- import Data.List (intercalate)
-- import Data.Map (Map)
-- import qualified Data.Map as Map

-- data GraphValue = S String | Ss [String] | I Int | B Bool | Null
--     deriving (Eq, Show)

-- printOutput :: [[(String, GraphValue)]] -> IO ()
-- printOutput nodes = do
--     let tables = groupNodesToTables nodes
--     mapM_ printTable (Map.elems tables)

-- printTable :: [(String, String)] -> IO ()
-- printTable rows = do
--     let header = fst (head rows)
--     putStrLn header
--     mapM_ (putStrLn . snd) rows
--     putStrLn ""

-- printRow :: Row -> IO ()
-- printRow (Header types) = 
--     do 
--         let id = ":ID"
--         let typesStr = typesToString types
--         putStrLn $ id ++ typesStr
-- printRow (LabeledHeader types) =
--     do
--         let id = ":ID"
--         let typesStr = typesToString types
--         let label = ", :LABEL"
--         putStrLn $ id ++ typesStr ++ label
-- printRow (RelationshipHeader types) = 
--     do 
--         let start = ":START_ID"
--         let typesStr = typesToString types
--         let end = ", :END_ID"
--         let typeStr = ", :TYPE"
--         putStrLn $ start ++ typesStr ++ end ++ typeStr
-- printRow (Data id values) = 
--     do
--         let idStr = idToString id
--         let valuesStr = valuesToString values
--         putStrLn $ idStr ++ valuesStr
-- printRow (LabeledData id values labels) = 
--     do
--         let idStr = idToString id
--         let valuesStr = valuesToString values
--         let labelsStr = labelsToString labels
--         putStrLn $ idStr ++ valuesStr ++ labelsStr
-- printRow (RelationshipData start values end relationship) =
--     do
--         let startStr = idToString start
--         let valuesStr = valuesToString values
--         let endStr = idToString end
--         let relationshipStr = ", " ++ relationship
--         putStrLn $ startStr ++ valuesStr ++ endStr ++ relationshipStr

-- typesToString :: Types -> String
-- typesToString (t:types) = 
--     case t of
--         (IntType value) -> 
--             ", " ++ value ++ ":integer" ++ typesToString types
--         (StringType value) -> 
--             ", " ++ value ++ ":string" ++ typesToString types
--         (BoolType value) -> 
--             ", " ++ value ++ ":boolean" ++ typesToString types
-- typesToString [] = ""

-- idToString :: ID -> String
-- idToString (Id value) = value

-- valuesToString :: [Value] -> String
-- valuesToString (v:values) = 
--     case v of
--         (IntValue value) -> 
--             ", " ++ show value ++ valuesToString values
--         (StringValue value) -> 
--             ", \"" ++ value ++ "\"" ++ valuesToString values
--         (BoolValue value) -> 
--             ", " ++ show value ++ valuesToString values
--         NullValue -> 
--             ", null" ++ valuesToString values
-- valuesToString [] = ""

-- labelsToString :: Labels -> String
-- labelsToString (l:labels) = ", " ++ l ++ labelsToString labels
-- labelsToString [] = ""

-- printTables :: [[(String, GraphValue)]] -> IO ()
-- printTables nodes = do
--     let tables = groupNodesToTables nodes
--     mapM_ printTable' (Map.elems tables)
--     where
--         printTable' rows = do
--             let header = fst $ head rows
--             putStrLn header
--             mapM_ (putStrLn . snd) rows
--             putStrLn ""

-- groupNodesToTables :: [[(String, GraphValue)]] -> Map String [(String, String)]
-- groupNodesToTables nodes = Map.fromListWith (++) [(getHeader node, [(getHeader node, nodeToRow node)]) | node <- nodes]
--     where
--         getHeader = intercalate "," . map fst . takeWhile (\(k, _) -> k /= ":LABEL")
--         nodeToRow = intercalate "," . map (valueToString . snd)
--         valueToString (I x) = show x
--         valueToString (S x) = x
--         valueToString (Ss xs) = intercalate ";" xs
--         valueToString (B True) = "true"
--         valueToString (B False) = "false"
--         valueToString Null = "null"

module Printer (printOutput, printTables, printRow, groupNodesToTables, GraphValue(..)) where

import InputParser
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

data GraphValue = S String | Ss [String] | I Int | B Bool | Null
    deriving (Eq, Show)

printOutput :: [[(String, String, GraphValue)]] -> IO ()
printOutput nodes = do
    let attributeTypes = getAttributeTypes nodes
    let nodeRows = map nodeToRow nodes
    let tables = groupNodesToTables attributeTypes nodeRows
    mapM_ printTable (Map.elems tables)

printTable :: ([(String, String)], [[(String, String)]]) -> IO ()
printTable (header, rows) = do
    let headerStr = intercalate "," $ map (\(k, t) -> k ++ ":" ++ t) header
    putStrLn headerStr
    mapM_ (putStrLn . intercalate ",") rows
    putStrLn ""

printRow :: Row -> IO ()
printRow (Header types) = 
    do 
        let id = ":ID"
        let typesStr = typesToString types
        putStrLn $ id ++ typesStr
printRow (LabeledHeader types) =
    do
        let id = ":ID"
        let typesStr = typesToString types
        let label = ", :LABEL"
        putStrLn $ id ++ typesStr ++ label
printRow (RelationshipHeader types) = 
    do 
        let start = ":START_ID"
        let typesStr = typesToString types
        let end = ", :END_ID"
        let typeStr = ", :TYPE"
        putStrLn $ start ++ typesStr ++ end ++ typeStr
printRow (Data id values) = 
    do
        let idStr = idToString id
        let valuesStr = valuesToString values
        putStrLn $ idStr ++ valuesStr
printRow (LabeledData id values labels) = 
    do
        let idStr = idToString id
        let valuesStr = valuesToString values
        let labelsStr = labelsToString labels
        putStrLn $ idStr ++ valuesStr ++ labelsStr
printRow (RelationshipData start values end relationship) =
    do
        let startStr = idToString start
        let valuesStr = valuesToString values
        let endStr = idToString end
        let relationshipStr = ", " ++ relationship
        putStrLn $ startStr ++ valuesStr ++ endStr ++ relationshipStr

typesToString :: Types -> String
typesToString (t:types) = 
    case t of
        (IntType value) -> 
            ", " ++ value ++ ":integer" ++ typesToString types
        (StringType value) -> 
            ", " ++ value ++ ":string" ++ typesToString types
        (BoolType value) -> 
            ", " ++ value ++ ":boolean" ++ typesToString types
typesToString [] = ""

idToString :: ID -> String
idToString (Id value) = value

valuesToString :: [Value] -> String
valuesToString (v:values) = 
    case v of
        (IntValue value) -> 
            ", " ++ show value ++ valuesToString values
        (StringValue value) -> 
            ", \"" ++ value ++ "\"" ++ valuesToString values
        (BoolValue value) -> 
            ", " ++ show value ++ valuesToString values
        NullValue -> 
            ", null" ++ valuesToString values
valuesToString [] = ""

labelsToString :: Labels -> String
labelsToString (l:labels) = ", " ++ l ++ labelsToString labels
labelsToString [] = ""

getAttributeTypes :: [[(String, String, GraphValue)]] -> [(String, String)]
getAttributeTypes nodes = Map.toList $ Map.fromListWith (\t1 t2 -> t1) [(k, t) | node <- nodes, (k, t, _) <- node]

nodeToRow :: [(String, String, GraphValue)] -> [(String, String)]
nodeToRow node = [(k, valueToString v) | (k, _, v) <- node]
    where
        valueToString (I x) = show x
        valueToString (S x) = x
        valueToString (Ss xs) = intercalate ";" xs
        valueToString (B True) = "true"
        valueToString (B False) = "false"
        valueToString Null = "null"

groupNodesToTables :: [(String, String)] -> [[(String, String)]] -> Map String ([(String, String)], [[(String, String)]])
groupNodesToTables attributeTypes nodeRows = Map.fromListWith mergeRows [(getHeader row, ([getAttribute k t | (k, t) <- attributeTypes], [row])) | row <- nodeRows]
    where
        getHeader = intercalate "," . map fst
        getAttribute k t = (k, t)
        mergeRows (h1, r1) (_, r2) = (h1, r1 ++ r2)