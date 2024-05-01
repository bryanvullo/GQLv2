module Printer (printOutput, printTables, printRow, groupNodesToTables, GraphValue(..)) where

import InputParser
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

data GraphValue = S String | Ss [String] | I Int | B Bool | Null
    deriving (Eq, Show)

printOutput :: [[(String, GraphValue)]] -> IO ()
printOutput nodes = do
    let typePairs = getAttrTypePair $ map (map (\(a, v) -> (a, "", v))) nodes
    let tables = groupNodesToTables typePairs nodes
    mapM_ printTable (Map.elems tables)

printTable :: [(String, String)] -> IO ()
printTable [] = return () -- Handle empty list case
printTable rows@((header, _):_) = do -- Pattern match on non-empty list
    putStrLn header
    mapM_ (putStrLn . snd) rows
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

printTables :: [[(String, String, GraphValue)]] -> IO ()
printTables nodes = do
    let typePairs = getAttrTypePair nodes
    let originalNodes = map (map (\(a,_,v) -> (a,v))) nodes
    let tables = groupNodesToTables typePairs originalNodes
    mapM_ printTable' (Map.elems tables)
    where
        printTable' [] = return () -- Handle empty list case
        printTable' rows@((header, _):_) = do -- Pattern match on non-empty list
            putStrLn header
            mapM_ (putStrLn . snd) rows
            putStrLn ""

getAttrTypePair :: [[(String, String, GraphValue)]] -> [(String, String)]
getAttrTypePair nodes = pairs
    where
        pairs = [(attr, aType) | (attr, aType, value) <- concat nodes,
            value /= Null,
            not (any (\(a, _) -> a == attr) pairs)]

groupNodesToTables :: [(String, String)] -> [[(String, GraphValue)]] -> Map String [(String, String)]
groupNodesToTables typePairs nodes = Map.fromListWith (++) [(getHeader node typePairs, [(getHeader node typePairs, nodeToRow node)]) | node <- nodes]
    where
        getHeader node typePairs = intercalate ", " $ map (\(k, v) -> k ++ ":" ++ getType k v typePairs) $ takeWhile (\(k, _) -> k /= ":LABEL") node
        getType k v typePairs = case lookup k typePairs of
            Just t -> t
            Nothing -> case v of
                I _ -> "integer"
                S _ -> "string"
                Ss _ -> "string"
                B _ -> "boolean"
                Null -> ""
        nodeToRow = intercalate ", " . map (valueToString . snd)
        valueToString (I x) = show x
        valueToString (S x) = x
        valueToString (Ss xs) = intercalate ";" xs
        valueToString (B True) = "true"
        valueToString (B False) = "false"
        valueToString Null = "null"