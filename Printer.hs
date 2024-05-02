module Printer (printTables, groupNodesToTables, GraphValue(..)) where

import InputParser
import Data.List (intercalate, nub)
import Data.Map (Map)
import qualified Data.Map as Map

data GraphValue = S String | Ss [String] | I Int | B Bool | Null | ID String
    deriving (Eq, Show)

printTables :: [[(String, String, GraphValue)]] -> IO ()
printTables nodes = do
    let noDupeNodes = nub nodes
    let typePairs = getAttrTypePair nodes
    let originalNodes = map (map (\(a,_,v) -> (a,v))) noDupeNodes
    let tables = groupNodesToTables typePairs originalNodes
    mapM_ printTable' (Map.elems tables)
    where
        printTable' [] = return ()
        printTable' rows@((header, _):_) = do
            putStrLn header
            mapM_ (putStrLn . snd) rows
            putStrLn ""

getAttrTypePair :: [[(String, String, GraphValue)]] -> [(String, String)]
getAttrTypePair nodes = pairs
    where
        pairs = [(attr, aType) | (attr, aType, value) <- concat nodes,
            value /= Null]
            --not (any (\(a, _) -> a == attr) pairs)

groupNodesToTables :: [(String, String)] -> [[(String, GraphValue)]] -> Map String [(String, String)]
groupNodesToTables typePairs nodes = Map.fromListWith (++) [(getHeader node typePairs, [(getHeader node typePairs, nodeToRow node)]) | node <- nodes]
    where
        getHeader node typePairs = intercalate ", " $ map (\(k, v) -> makeHeaderString k v typePairs) node -- $ takeWhile (\(k, _) -> k /= ":LABEL")
        nodeToRow = intercalate ", " . map (valueToString . snd)
        valueToString (I x) = show x
        valueToString (S x) = "\"" ++ x ++ "\""
        valueToString (ID x) = x
        valueToString (Ss xs) = intercalate "; " xs
        valueToString (B True) = "true"
        valueToString (B False) = "false"
        valueToString Null = "null"

getType :: String -> GraphValue -> [(String, String)] -> String
getType k v typePairs = case lookup k typePairs of
            Just t -> t
            Nothing -> case v of
                I _ -> "integer"
                S _ -> "string"
                Ss _ -> "string"
                B _ -> "boolean"
                Null -> ""

makeHeaderString :: String -> GraphValue -> [(String, String)] -> String 
makeHeaderString "LABEL" _ _ = ":LABEL"
makeHeaderString "TYPE" _ _ = ":TYPE"
makeHeaderString "ID" _ _ = ":ID"
makeHeaderString "START_ID" _ _ = ":START_ID"
makeHeaderString "END_ID" _ _ = ":END_ID"
makeHeaderString k v typePairs = k ++ ":" ++ getType k v typePairs