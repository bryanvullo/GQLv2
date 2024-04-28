module Parsing.N4jParser where
import Lexing.N4jLexer
import Data.Char (toLower)

semiCombineHelper :: [String] -> String -> String
semiCombineHelper (s:ss) acc = semiCombineHelper ss (acc ++ ";" ++ s)
semiCombineHelper [] acc = acc

semiCombine :: [String] -> String
semiCombine (s:ss) = semiCombineHelper ss s

data NodeFieldTypes = IntField Int | StringField [String] | BoolField Bool | NullField
  deriving(Eq)
instance Show NodeFieldTypes where
  show (IntField n) = show n
  show (StringField s) = semiCombine s
  show (BoolField b) = map toLower $ show b
  show (NullField) = "null"
instance Ord NodeFieldTypes where
  (IntField n1) `compare` (IntField n2) = n1 `compare` n2
  _ `compare` _ = error "Invalid Comparison"

data LabelType
  = StringLabel
  | IntLabel
  | BoolLabel
  | NullLabel -- for big fields
  deriving(Eq, Show)

type Graph = ([NodeGroup], [Relationship])

type Relationship = NodeGroup
type NodeGroup = (Headers, [Node])
type Node = [(String, NodeFieldTypes)]

type Headers = [(String, LabelType)]

semiSplitHelper :: String -> String -> [String] -> [String]
semiSplitHelper (';':ss) s2 split = semiSplitHelper ss "" (split ++ [s2])
semiSplitHelper (s:ss) s2 split = semiSplitHelper ss (s2 ++ [s]) split
semiSplitHelper [] s2 split = split ++ [s2]

semicolonSplit :: String -> [String]
semicolonSplit s = semiSplitHelper s "" []

parseNodeHeaders :: [Token] -> Headers -> Graph -> Graph
parseNodeHeaders ((Tn4j TokenComma _):(Tn4j TokenColon _):(Tn4j (TokenField label) _):ts) headers graph = parseNodeHeaders ts (headers ++ [(label, NullLabel)]) graph
parseNodeHeaders ((Tn4j TokenComma _):(Tn4j (TokenField label) _):(Tn4j TokenColon _):(Tn4j (TokenField "string") _):ts) headers graph = parseNodeHeaders ts (headers ++ [(label, StringLabel)]) graph
parseNodeHeaders ((Tn4j TokenComma _):(Tn4j (TokenField label) _):(Tn4j TokenColon _):(Tn4j (TokenField "integer") _):ts) headers graph = parseNodeHeaders ts (headers ++ [(label, IntLabel)]) graph
parseNodeHeaders ((Tn4j TokenComma _):(Tn4j (TokenField label) _):(Tn4j TokenColon _):(Tn4j (TokenField "boolean") _):ts) headers graph = parseNodeHeaders ts (headers ++ [(label, BoolLabel)]) graph
parseNodeHeaders tokens headers graph = parseNode tokens headers [] [] headers graph

parseNode :: [Token] -> Headers -> Node -> [Node] -> Headers -> Graph -> Graph
parseNode ((Tn4j TokenComma _):ts) headers node nodes savedHeaders graph = parseNode ts headers node nodes savedHeaders graph
parseNode ((Tn4j TokenNull _):ts) ((label,_):hs) node nodes savedHeaders graph = parseNode ts hs (node ++ [(label, NullField)]) nodes savedHeaders graph
parseNode ((Tn4j (TokenString field) _):ts) ((label,StringLabel):hs) node nodes savedHeaders graph = parseNode ts hs (node ++ [(label,StringField [field])]) nodes savedHeaders graph
parseNode ((Tn4j (TokenInt field) _):ts) ((label,IntLabel):hs) node nodes savedHeaders graph = parseNode ts hs (node ++ [(label,IntField field)]) nodes savedHeaders graph
parseNode ((Tn4j TokenTrue _):ts) ((label,BoolLabel):hs) node nodes savedHeaders graph = parseNode ts hs (node ++ [(label,BoolField True)]) nodes savedHeaders graph
parseNode ((Tn4j TokenFalse _):ts) ((label,BoolLabel):hs) node nodes savedHeaders graph = parseNode ts hs (node ++ [(label,BoolField False)]) nodes savedHeaders graph
parseNode ((Tn4j (TokenField field) _):ts) ((label,NullLabel):hs) node nodes savedHeaders graph = parseNode ts hs (node ++ [(label,StringField (semicolonSplit field))]) nodes savedHeaders graph
parseNode ((Tn4j TokenColon apos):ts) [] node nodes headers (nodeGroups, relationships) = parseRecurse ((Tn4j TokenColon apos):ts) ((nodeGroups ++ [(headers, (nodes ++ [node]))]), relationships)
parseNode ((Tn4j (TokenField field) apos):ts) [] node nodes headers graph = parseNode ((Tn4j (TokenField field) apos):ts) headers [] (nodes ++ [node]) headers graph
parseNode [] _ node nodes headers (nodeGroups, relationships) = parseRecurse [] ((nodeGroups ++ [(headers, (nodes ++ [node]))]), relationships)
parseNode (t:tn) _ node nodes headers graph = error $ "Parse Node: " ++ show graph ++ " token: " ++ show t

parseRelationHeaders :: [Token] -> Headers -> Graph -> Graph
parseRelationHeaders ((Tn4j TokenComma _):(Tn4j TokenColon _):(Tn4j (TokenField label) _):ts) headers graph = parseRelationHeaders ts (headers ++ [(label, NullLabel)]) graph
parseRelationHeaders ((Tn4j TokenComma _):(Tn4j (TokenField label) _):(Tn4j TokenColon _):(Tn4j (TokenField "string") _):ts) headers graph = parseRelationHeaders ts (headers ++ [(label, StringLabel)]) graph
parseRelationHeaders ((Tn4j TokenComma _):(Tn4j (TokenField label) _):(Tn4j TokenColon _):(Tn4j (TokenField "integer") _):ts) headers graph = parseRelationHeaders ts (headers ++ [(label, IntLabel)]) graph
parseRelationHeaders ((Tn4j TokenComma _):(Tn4j (TokenField label) _):(Tn4j TokenColon _):(Tn4j (TokenField "boolean") _):ts) headers graph = parseRelationHeaders ts (headers ++ [(label, BoolLabel)]) graph
parseRelationHeaders tokens headers graph = parseRelation tokens headers [] [] headers graph

parseRelation :: [Token] -> Headers -> Node -> [Node] -> Headers -> Graph -> Graph
parseRelation ((Tn4j TokenComma _):ts) headers node nodes savedHeaders graph = parseRelation ts headers node nodes savedHeaders graph
parseRelation ((Tn4j TokenNull _):ts) ((label,_):hs) node nodes savedHeaders graph = parseRelation ts hs (node ++ [(label, NullField)]) nodes savedHeaders graph
parseRelation ((Tn4j (TokenField field) _):ts) ((label,StringLabel):hs) node nodes savedHeaders graph = parseRelation ts hs (node ++ [(label,StringField [field])]) nodes savedHeaders graph
parseRelation ((Tn4j (TokenInt field) _):ts) ((label,IntLabel):hs) node nodes savedHeaders graph = parseRelation ts hs (node ++ [(label,IntField field)]) nodes savedHeaders graph
parseRelation ((Tn4j TokenTrue _):ts) ((label,BoolLabel):hs) node nodes savedHeaders graph = parseRelation ts hs (node ++ [(label,BoolField True)]) nodes savedHeaders graph
parseRelation ((Tn4j TokenFalse _):ts) ((label,BoolLabel):hs) node nodes savedHeaders graph = parseRelation ts hs (node ++ [(label,BoolField False)]) nodes savedHeaders graph
parseRelation ((Tn4j (TokenField field) _):ts) ((label,NullLabel):hs) node nodes savedHeaders graph = parseRelation ts hs (node ++ [(label,StringField (semicolonSplit field))]) nodes savedHeaders graph
parseRelation ((Tn4j TokenColon apos):ts) [] node nodes headers (nodeGroups, relationships) = parseRecurse ((Tn4j TokenColon apos):ts) (nodeGroups, (relationships ++ [(headers, (nodes ++ [node]))]))
parseRelation ((Tn4j (TokenField field) apos):ts) [] node nodes headers graph = parseRelation ((Tn4j (TokenField field) apos):ts) headers [] (nodes ++ [node]) headers graph
parseRelation [] _ node nodes headers (nodeGroups, relationships) = parseRecurse [] (nodeGroups, (relationships ++ [(headers, (nodes ++ [node]))]))
parseRelation (t:tn) _ node nodes headers graph = error $ "Parse Relation: " ++ show graph ++ " token: " ++ show t

parseRecurse :: [Token] -> Graph -> Graph
parseRecurse ((Tn4j TokenColon _):(Tn4j (TokenField "ID") _):ts) graph = parseNodeHeaders ts [("ID", NullLabel)] graph
parseRecurse ((Tn4j TokenColon _):(Tn4j (TokenField "START_ID") _):ts) graph = parseRelationHeaders ts [("START_ID", NullLabel)] graph
parseRecurse [] graph = graph
parseRecurse (t:tn) graph = error $ show graph

parseToGraph :: [Token] -> Graph
parseToGraph tokens = parseRecurse tokens ([],[])

---------------------------------------------

{- hasDuplicates :: [String] -> Bool
hasDuplicates xs = length xs /= length (Set.fromList xs)

getIDfromNode :: Node -> String
getIDfromNode (("ID", nft):ns) = show nft
getIDfromNode (n:ns) = getIDfromNode ns
getIDfromNode [] = error $ "A node has no ID"

getListOfNodeIDsNodes :: [Node] -> [String] -> [String]
getListOfNodeIDs (n:ns) acc = getListOfNodeIDsNodes ns (acc ++ [getIDfromNode n])
getListOfNodeIDs [] acc = acc

getListOfNodeIDs :: [NodeGroup] -> [String] -> [String]
getListOfNodeIDs ((_,n):ns) acc = getListOfNodeIDs ns (acc ++ getListOfNodeIDsNodes n)

checkIfIDsUnique :: Graph -> Bool
checkIfIDsUnique (nodes,_) = do
  let idList = getListOfNodeIDs nodes
  if hasDuplicates idList
    then error $ "There are duplicate IDs in this graph"
    else True 

checkIfAllIDsDifferent :: Graph -> Bool
checkIfAllIDsDifferent 

graphCheck :: Graph -> Bool
graphCheck g = checkIfIDsUnique g -}