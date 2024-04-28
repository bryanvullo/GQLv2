module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import System.Exit (exitFailure)
import Control.Exception (catch, IOException)
import Lexing.Tokens
import Lexing.N4jLexer
import Parsing.N4jParser
-- import TypeChecking.Types
import Data.List (intersect, nub)
import Data.Maybe (mapMaybe, isNothing, catMaybes)
import Parsing.Grammar
import System.IO.Unsafe(unsafePerformIO)
import Text.Regex.TDFA
import Debug.Trace
import Data.Set(fromList, toList)
import Parsing.N4jParser (NodeFieldTypes(NullField))


-- This utility function processes the given file and handles errors
-- processFile :: FilePath -> FilePath -> IO ()
-- processFile inputFile queryFile = do
--   inputContent <- readFile inputFile `catch` handleFileReadError
--   queryContent <- readFile queryFile `catch` handleFileReadError
--   case (parseContent inputContent, parseQuery queryContent) of
--     (Right parsedGraph, Right parsedQuery) -> do
--       let evaluatedGraph = evaluate parsedGraph parsedQuery
--       printGraph evaluatedGraph
--       putStrLn "File processed successfully."
--     (Left parseError, _) -> handleParseError parseError
--     (_, Left parseError) -> handleParseError parseError
--   where
--     handleFileReadError :: IOException -> IO ()
--     handleFileReadError _ = do
--       putStrLn "Error: Unable to read file"
--       exitFailure

--     handleParseError :: ParseError -> IO ()
--     handleParseError (ParseError parseError) = do
--       putStrLn ("Parse error: " ++ parseError)
--       exitFailure

--     printGraph :: Graph -> IO ()
--     printGraph graph = do
--       putStrLn "Evaluated Graph:"
--       print graph

-- This function parses the content of the input file and returns either a parse error or the parsed graph
-- parseContent :: String -> Either ParseError Graph
-- parseContent input =
--   case parse (alexScanTokens input) of
--     Left parseError -> Left (ParseError (show parseError))
--     Right parsedGraph -> Right parsedGraph

type Environment = [(String, Out)]
type Environments = [Environment] -- stack of environments

data Out = Graph Graph | Node (Node, Headers) | Relation (Node, Headers) | Field NodeFieldTypes | Regex String

-- type Node = [(String, NodeFieldTypes)]
nodeToEnvironment :: Node -> Environment
nodeToEnvironment = map fieldAttach
  where
    fieldAttach :: (String, NodeFieldTypes) -> (String, Out)
    fieldAttach (s, f) = (s, Field f)

instance Eq Out where
  (Graph a) == (Graph b) = a == b
  (Node (a, _)) == (Node (b, _)) = a == b
  (Relation (a, _)) == (Relation (b, _)) = a == b
  (Field a) == (Field b) = a == b
  (Main.Regex a) == (Main.Regex b) = a == b
  (Main.Regex a) == (Field (StringField [b])) = b =~ a :: Bool
  (Field (StringField [a])) == (Main.Regex b) = a =~ b :: Bool
  _ == _ = False

getFieldFromNode :: Node -> String -> Maybe NodeFieldTypes
getFieldFromNode ((str,nft):ns) string = do
  if str == string
    then return nft
    else getFieldFromNode ns string
getFieldFromNode [] str = Nothing

aostsHelper :: [String] -> String -> String
aostsHelper (s:ss) acc = aostsHelper ss (acc ++ ";" ++ s)
aostsHelper [] acc = acc

arrayOfStringToString :: [String] -> String
arrayOfStringToString (s:ss) = aostsHelper ss s

showNode :: Node -> [String] -> String -> String
showNode node (h:[]) acc = acc ++ (show (o))
  where
    (Just o) = getFieldFromNode node h
showNode node (h:hs) acc = showNode node hs (acc ++ (show (o)) ++ ", ")
  where
    (Just o) = getFieldFromNode node h

showNodes :: [Node] -> [String] -> String -> String
showNodes (n:ns) headers acc = showNodes ns headers (acc ++ (showNode n headers "") ++ "\n")
showNodes [] headers acc = acc

showNodeHeader :: Headers -> (String, [String]) -> (String, [String])
showNodeHeader ((str, StringLabel):hs) (acc,ss) = showNodeHeader hs ((acc ++ ", " ++ str ++ ":string"),(ss ++ [str]))
showNodeHeader ((str, IntLabel):hs) (acc,ss) = showNodeHeader hs ((acc ++ ", " ++ str ++ ":integer"),(ss ++ [str]))
showNodeHeader ((str, BoolLabel):hs) (acc,ss) = showNodeHeader hs ((acc ++ ", " ++ str ++ ":boolean"),(ss ++ [str]))
showNodeHeader (("ID", NullLabel):hs) (acc,ss) = showNodeHeader hs ((":ID" ++ acc),(["ID"] ++ ss))
showNodeHeader (("LABEL", NullLabel):[]) (acc,ss) = ((acc ++ ", :LABEL"),(ss ++ ["LABEL"]))
showNodeHeader (("LABEL", NullLabel):hs) acc = showNodeHeader (hs ++ [("LABEL", NullLabel)]) acc
showNodeHeader [] (acc,ss) = (acc,ss)
-- showNodeHeader a b = {- trace (show a) -} b

showNodeGroup :: NodeGroup -> String
showNodeGroup (headers,nodes) = sh ++ "\n" ++ (showNodes nodes shh "")
  where
    (sh,shh) = showNodeHeader headers ("",[])

showNodeGroups :: [NodeGroup] -> String -> String
showNodeGroups (n:ns) acc = showNodeGroups ns (acc ++ "\n" ++ (showNodeGroup n))
showNodeGroups [] acc = acc

---------------------------------------------

showRelation :: Node -> [String] -> String -> String
showRelation node (h:[]) acc = acc ++ (show (o))
  where
    (Just o) = getFieldFromNode node h
showRelation node (h:hs) acc = showRelation node hs (acc ++ (show (o)) ++ ", ")
  where
    (Just o) = getFieldFromNode node h

showRelations :: [Node] -> [String] -> String -> String
showRelations (n:ns) headers acc = showRelations ns headers (acc ++ (showRelation n headers "") ++ "\n")
showRelations [] headers acc = acc

showRelationHeader :: Headers -> (String, [String]) -> (String, [String])
showRelationHeader ((str, StringLabel):hs) (acc,ss) = showRelationHeader hs ((acc ++ ", " ++ str ++ ":string"),(ss ++ [str]))
showRelationHeader ((str, IntLabel):hs) (acc,ss) = showRelationHeader hs ((acc ++ ", " ++ str ++ ":integer"),(ss ++ [str]))
showRelationHeader ((str, BoolLabel):hs) (acc,ss) = showRelationHeader hs ((acc ++ ", " ++ str ++ ":boolean"),(ss ++ [str]))
showRelationHeader (("START_ID", NullLabel):hs) (acc,ss) = showRelationHeader hs ((":START_ID" ++ acc),(["START_ID"] ++ ss))
showRelationHeader (("END_ID", NullLabel):("TYPE",NullLabel):[]) (acc,ss) = ((acc ++ ", :END_ID, :TYPE"),(ss ++ ["END_ID", "TYPE"]))
showRelationHeader (("END_ID", NullLabel):hs) acc = showNodeHeader (hs ++ [("END_ID", NullLabel)]) acc
showRelationHeader (("TYPE", NullLabel):hs) acc = showNodeHeader (hs ++ [("TYPE", NullLabel)]) acc
showRelationHeader [] (acc,ss) = (acc,ss)

showRelationNodeGroup :: NodeGroup -> String
showRelationNodeGroup (headers,nodes) = sh ++ "\n" ++ (showRelations nodes shh "")
  where
    (sh,shh) = showRelationHeader headers ("",[])

showRelationNodeGroups :: [NodeGroup] -> String -> String
showRelationNodeGroups (n:ns) acc = showRelationNodeGroups ns (acc ++ "\n" ++ (showRelationNodeGroup n))
showRelationNodeGroups [] acc = acc

-----------------------------------------------

showGraph :: Graph -> String
showGraph (ns,rs) = (showNodeGroups ns "") ++ "\n\n" ++ (showRelationNodeGroups rs "")

instance Show Out where
  show (Graph g) = showGraph g
  show (Node n) = show n
  show (Relation r) = show r
  show (Field f) = show f
  show (Main.Regex s) = show $ "regex:" ++ s

instance Ord Out where
  (Field f1) `compare` (Field f2) = f1 `compare` f2
  _ `compare` _ = runtimeError "Invalid Comparison"

addToEnvironments :: String -> Out -> Environments -> Environments
addToEnvironments _ _ [] = runtimeError "Not within Environment"
addToEnvironments s out (env:envs) = addToEnvironment s out env : envs

-- add (string, out) to an environment or override existing assignment
addToEnvironment :: String -> Out -> Environment -> Environment
addToEnvironment s out env | isNothing (lookupEnvironment s env) = (s, out) : env -- variable does not already exist in environment
                           | otherwise = updateVar' s out env

declareToEnvironment :: String -> Type -> Environments -> Environments
declareToEnvironment s (Type (Tn TokenGraphType _)) (env:envs) = ((s, Graph ([], [])) : env) : envs
declareToEnvironment _ _ _ = runtimeError "Can only declare Empty Graphs"

--updating variable in entire environment
updateVar :: String -> Out -> Environments -> Environments
updateVar s out (env:envs) | isNothing (lookupEnvironment s env) = env : updateVar s out envs
                           | otherwise = updateVar' s out env : envs

--updating variable in single environment  
updateVar' :: String -> Out -> Environment -> Environment
updateVar' _ _ [] = runtimeError "Variable is not declared in environment" -- no variable found in environment
updateVar' s out (e@(k, _):env) | k == s = (k, out) : env
                              | otherwise = e : updateVar' s out env

--lookup variable name in a single environment
--returns Nothing if variable is not in environment
lookupEnvironment :: String -> Environment -> Maybe Out
lookupEnvironment s env | null match = Nothing
                        | otherwise = Just (snd $ head match)
  where
    match = filter ((==) s . fst) env

--lookup variable name in entire environment stack
--returns 
lookupEnvironments :: String -> Environments -> Maybe Out
lookupEnvironments s envs | null match = Nothing
                          | otherwise = head match
  where
    match = filter (Nothing /=) $ map (lookupEnvironment s) envs

--pr1.gql:
--[Expr (TypedAssign (Type (Tn TokenGraphType (AlexPn 0 1 1))) "graph" (ReadFile "\"./access.n4j\"")),Expr (Assign (Var "graph") (MatchQuery (Var "graph") (Or (Equals (Var "\"LABEL\"") (String "\"Visitor\"")) (LTEquals (Var "age") (MathExpr (Int 25)))))),Expr (Print "graph")]

run :: Program -> Environments -> Environments
run [] envs = envs
run p@(IfBlock _ _ : _) envs = embeddedRun p envs
run p@(IfElseBlock _ _ _ : _) envs = embeddedRun p envs
run p@(ForBlock _ _ _ _ : _) envs = embeddedRun p envs
run (s:ss) envs = run ss envs'
  where
    envs' = execute s envs

-- for multiple closure operations like if/else blocks and for loops
embeddedRun :: Program -> Environments -> Environments
embeddedRun (s:ss) envs = run ss envs'
  where
    (_:envs') = execute s ([] : envs)

--add new environment on environment stack when entering if or for block for scoping
execute :: Statement -> Environments -> Environments
execute (Expr e) envs = snd $ exprReduction e envs
execute (IfBlock b p) envs | b1 = run p envs1
                           | otherwise = envs
                              where
                                (b1, envs1) = boolReduction b envs
execute (IfElseBlock b p1 p2) envs | b1 = run p1 envs1
                                   | otherwise = run p2 envs1
                                    where
                                      (b1, envs1) = boolReduction b envs
execute (ForBlock (Type (Tn t _)) s e p) envs = case t of
  TokenNodeType -> foldl assignAndRunNGS envs nds
  TokenRelationType -> foldl assignAndRunNGS envs rs
  where
    (Just (Graph (nds, rs)), envs1) = exprReduction e envs
    assignAndRunNGS :: Environments -> NodeGroup -> Environments
    assignAndRunNGS envs' (hds, nds) = foldl (assignAndRun hds) envs' nds
    assignAndRun :: Headers -> Environments -> Node -> Environments
    assignAndRun hds (env:envs') n = run p $ case lookupEnvironment s env of
      Nothing -> addToEnvironment s (Node (n, hds)) env : envs'
      _ -> updateVar' s (Node (n,hds)) env : envs'

execute (Print s) envs = unsafePerformIO $ do
  let out = lookupEnvironments s envs
  case out of
    (Just o) -> do
      putStrLn $ show o
    Nothing -> runtimeError "Unprintable due to no variable found"
  return envs


exprReduction :: Expr -> Environments -> (Maybe Out, Environments)
exprReduction (MathExpr me) envs = (Just $ Field $ IntField $ mathReduction me envs, envs)
exprReduction (String s) envs = (Just $ Field $ StringField [s], envs)
exprReduction (Parsing.Grammar.Regex s) envs = (Just (Main.Regex s), envs)
exprReduction (MatchQuery s be) envs = (Just $ Graph $ matchGraph g be, envs)
  where
    (Just (Graph g)) = lookupEnvironments s envs
    matchGraph :: Graph -> BoolExpr -> Graph
    matchGraph (ndgs, rs) be = (matchNodeGroups ndgs be, matchNodeGroups rs be)
    matchNodeGroups :: [NodeGroup] -> BoolExpr -> [NodeGroup]
    matchNodeGroups ngs be = map (matchNodeGroup be) ngs
    matchNodeGroup :: BoolExpr -> NodeGroup -> NodeGroup
    matchNodeGroup be (h, nodes) = (h, matchNodes nodes be)
    matchNodes :: [Node] -> BoolExpr -> [Node]
    matchNodes ns be = filter (matchNode be) ns
    matchNode :: BoolExpr -> Node -> Bool
    matchNode be n = fst $ boolReduction be (nodeToEnvironment n : envs)
exprReduction (AddQuery s e) envs = {- trace (show envs) -} (Nothing, updateVar s (Graph newGraph) envs1)
  where
    (Just (Graph g)) = lookupEnvironments s envs
    (Just o, envs1) = exprReduction e envs
    newGraph = addToGraph ({- trace (show o) -} o) g
    addToGraph :: Out -> Graph -> Graph
    addToGraph (Node (n, hds)) (ndg,rs) = (addNodeGroups n hds ndg, rs)
    addToGraph (Relation (r, hds)) (ndg,rs) = (ndg, addNodeGroups r hds rs)
    addToGraph _ _ = runtimeError "Use of ADD: <Graph>.ADD(<Node>) or <Graph>.ADD(<Relation>)"
    addNodeGroups :: Node -> Headers -> [NodeGroup] -> [NodeGroup]
    addNodeGroups n hds [] = [(hds, [n])]
    addNodeGroups n hds (ndg:ndgs)        | match && same = (fst ndg, n : snd ndg) : ndgs
                                          | match = ndg : ndgs
                                          | otherwise = ndg : addNodeGroups n hds ndgs
      where
        nodeHeaders = map fst n
        headerHeaders = map fst (fst ndg)
        match = (fromList nodeHeaders) == (fromList headerHeaders)
        same = notElem n (snd ndg)

exprReduction (BoolExpr be) envs = (Just (Field (BoolField b)), envs')
  where
    (b, envs') = boolReduction be envs
exprReduction (GetRelation e be) envs = (Just (Graph matched), envs)
  where
    (Just (Graph (n,r))) = lookupEnvironments e envs
    matched = matchGraph ([],r) be 
    matchGraph :: Graph -> BoolExpr -> Graph
    matchGraph (ndgs, rs) be = (matchNodeGroups ndgs be, matchNodeGroups rs be)
    matchNodeGroups :: [NodeGroup] -> BoolExpr -> [NodeGroup]
    matchNodeGroups ngs be = map (matchNodeGroup be) ngs
    matchNodeGroup :: BoolExpr -> NodeGroup -> NodeGroup
    matchNodeGroup be (h, nodes) = (h, matchNodes nodes be)
    matchNodes :: [Node] -> BoolExpr -> [Node]
    matchNodes ns be = filter (matchNode be) ns
    matchNode :: BoolExpr -> Node -> Bool
    matchNode be n = fst $ boolReduction be (nodeToEnvironment n : envs)
exprReduction (Exclude e1 e2) envs = undefined
exprReduction (GetNode e1 e2) envs = (Just (Node matched), envs1)
  where
    (Just (Graph (ngs,_))) = lookupEnvironments e1 envs
    (Just o, envs1) = exprReduction e2 envs
    matched = searchNGsByID ngs (show (Just (o)))
    searchNGsByID :: [NodeGroup] -> String -> (Node, Headers)
    searchNGsByID (n:ns) id = case searchNodeGroupByID n id of
      (Nothing, _) -> searchNGsByID ns id
      (Just searched, headers) -> (searched, headers)
    searchNGsByID [] id = runtimeError "Cannot find node with ID in GETNODE"

    searchNodeGroupByID :: NodeGroup -> String -> (Maybe Node, Headers)
    searchNodeGroupByID (h,n) id = (searchNodesByID n id, h)

    searchNodesByID :: [Node] -> String -> Maybe Node
    searchNodesByID (n:ns) id
      | searched == Nothing = searchNodesByID ns id
      | otherwise = searched
      where
        searched = searchNodeByID n n id
    searchNodesByID [] id = Nothing

    searchNodeByID :: Node -> Node -> String -> Maybe Node
    searchNodeByID (("ID", nft):ns) node id 
      | show nft == id = Just node
      | otherwise = searchNodeByID ns node id
    searchNodeByID [] node id = Nothing
    
exprReduction (Assignable assignable) envs = assignableReduction assignable envs
exprReduction (AssignExpr assign) envs = (Nothing, assignReduction assign envs)

assignReduction :: AssignExpr -> Environments -> Environments
assignReduction (TypedAssign _ s e) envs = addToEnvironments s o1 envs1
  where
    (Just o1, envs1) = exprReduction e envs
assignReduction (Declare t s) envs = case t of
                                      (Type (Tn TokenGraphType _)) -> declareToEnvironment s t envs
                                      _ -> runtimeError "Empty Declaration only allowed for Graph"

assignReduction (Assign a e) envs = assignableUpdate a o envs1
  where
    (o, envs1) = case (exprReduction e envs) of
      (Just o', envs1') -> (o', envs1')
      (Nothing, envs1') -> (Field NullField, envs1')
assignReduction (RelationDeclare varName headers) envs = addToEnvironments varName (Relation  (generateNode headers, headers)) envs
assignReduction (NodeDeclare varName headers) envs = addToEnvironments varName (Node  (generateNode headers, headers)) envs

generateNode :: Headers -> Node
generateNode = map (\(s, lt) -> (s, NullField))

assignableUpdate :: Assignable -> Out -> Environments -> Environments
assignableUpdate (Var s) o envs = updateVar s o envs
assignableUpdate (GetProperty s1 s2) o envs = updateVar s1 (nodeUpdate o1 s2 f) envs
  where
    (Just o1) = lookupEnvironments s1 envs
    (Field f) = o
    -- temp = case o of
    --   (Field f) -> (nodeUpdate o1 s2 f)
    --   _ -> trace ("DEBUG " ++ show o) (Field (StringField ["NO"]))

nodeUpdate :: Out -> String -> NodeFieldTypes -> Out
nodeUpdate (Node (node, hds)) s f = Node (nodeUpdate' node s f, hds)
nodeUpdate (Relation (node, hds)) s f = Relation  (nodeUpdate' node s f, hds)

nodeUpdate' :: Node -> String -> NodeFieldTypes -> Node
nodeUpdate' [] s f = [(s, f)]
nodeUpdate' ((k, v) : dict) s f | k == s = (k, f) : dict
                                | otherwise = (k, v) : nodeUpdate' dict s f

assignableReduction :: Assignable -> Environments -> (Maybe Out, Environments)
assignableReduction (Var s) envs = (lookupEnvironments s envs, envs)
assignableReduction (GetProperty s1 s2) envs = (nodeSearch o s2, envs)
  where
    (Just o) = lookupEnvironments s1 envs

nodeSearch :: Out -> String -> Maybe Out
nodeSearch (Node (node, _)) s = nodeSearch' node s
nodeSearch (Relation (node, _)) s = nodeSearch' node s

nodeSearch' :: Node -> String -> Maybe Out
nodeSearch' [] _ = Nothing
nodeSearch' ((k, v) : dict) s | k == s = Just $ Field v
                              | otherwise = {-trace (show s ++ " " ++ show (k, v))-} nodeSearch' dict s

-- graphUpdate :: Graph -> String -> NodeFieldTypes -> Graph
-- graphUpdate g s f = undefined
-- graphUpdate _ _ _ = runtimeError "Invalid"

-- graphSearch :: Out -> String -> Out
-- graphSearch (Graph (ngs, rs)) s = output
--   where
--     first = lookupGraph ndgs s
--     second = lookupGraph rs s
--     output = case first of
--       Nothing -> case second of
--         Nothing -> Nothing
--         _ -> second
--       _ -> first
--     lookupGraph :: [NodeGroup] -> String -> Out
--     lookupGraph ndgs 
-- graphSearch _ _ = runtimeError "No Graph to Search"

mathReduction :: MathExpr -> Environments -> Int
mathReduction (Int n) _ = n
mathReduction (Addition n1 n2) env = mathReduction n1 env + mathReduction n2 env
mathReduction (Subtraction n1 n2) env = mathReduction n1 env - mathReduction n2 env
mathReduction (Multiplication n1 n2) env = mathReduction n1 env * mathReduction n2 env
mathReduction (Division n1 n2) env = mathReduction n1 env `div` mathReduction n2 env


boolReduction :: BoolExpr -> Environments -> (Bool, Environments)
boolReduction (Bool b) env = (b, env)
boolReduction (Equals e1 e2) envs = let (o1, o2, envs') = binaryExpressionReduction e1 e2 envs in (o1 == o2, envs')
boolReduction (NotEquals e1 e2) envs = let (o1, o2, envs') = binaryExpressionReduction e1 e2 envs in (o1 /= o2, envs')
boolReduction (LessThan e1 e2) envs = let (o1, o2, envs') = binaryExpressionReduction e1 e2 envs in (o1 < o2, envs')
boolReduction (GreaterThan e1 e2) envs = let (o1, o2, envs') = binaryExpressionReduction e1 e2 envs in (o1 > o2, envs')
boolReduction (LTEquals e1 e2) envs = let (o1, o2, envs') = binaryExpressionReduction e1 e2 envs in (o1 <= o2, envs')
boolReduction (GTEquals e1 e2) envs = let (o1, o2, envs') = binaryExpressionReduction e1 e2 envs in (o1 >= o2, envs')
boolReduction (And e1 e2) envs = let (o1, o2, envs') = binaryExpressionReduction e1 e2 envs in (o1 `outAnd` o2, envs')
boolReduction (Or e1 e2) envs = let (o1, o2, envs') = binaryExpressionReduction e1 e2 envs in (o1 `outOr` o2, envs')
boolReduction (EndRelationQuery be s1 s2) envs = relationQueryReduction s2 (And (BoolExpr be) (BoolExpr (Equals (Assignable (Var "END_ID")) (Assignable (Var s1))))) envs
boolReduction (StartRelationQuery s1 be s2) envs = relationQueryReduction s2 (And (BoolExpr be) (BoolExpr (Equals (Assignable (Var "START_ID")) (Assignable (Var s1))))) envs
boolReduction (RelationQuery s1 be s2 s3) envs = relationQueryReduction s3 (And (BoolExpr be) (BoolExpr $ And (BoolExpr (Equals (Assignable (Var "START_ID")) (Assignable (Var s1)))) (BoolExpr (Equals (Assignable (Var "END_ID")) (Assignable (Var s2)))))) envs
boolReduction (Contains e ss) envs = undefined

relationQueryReduction :: String -> BoolExpr -> Environments -> (Bool, Environments)
relationQueryReduction s be envs = check be environmentArr envs
  where
    (Just (Graph (_, rs))) = lookupEnvironments s envs
    environmentArr :: [Environment]
    environmentArr = map nodeToEnvironment (concatMap snd rs)
    check :: BoolExpr -> [Environment] -> Environments -> (Bool, Environments)
    check be [] envs = (False, envs)
    check be (e:es) envs | b = (b, envs)
                         | otherwise = check be es envs
      where
        (b, e') = boolReduction be (e : envs)

-----------------------graphVar
-- relationQueryReduction s be envs = (Nothing /= firstJust (map (relationQueryFilter be) rs), envs)
--   where
--     (Just (Graph (_, rs))) = lookupEnvironments s envs
--     relationQueryFilter :: BoolExpr -> Relationship -> Maybe Relationship
--     relationQueryFilter be (hs, nodes) | (length list) == 0 = Nothing
--                                        | otherwise = Just list
--       where
--         list = (hs, filter (filterFunc be) nodes)
--     firstJust :: [Maybe a] -> Maybe a
--     firstJust [] = Nothing  -- Base case: Empty list returns Nothing
--     firstJust xs = case catMaybes xs of
--         []    -> Nothing       -- If all values are Nothing, return Nothing
--         (y:_) -> Just y        -- Otherwise, return the first non-Nothing value
--     filterFunc :: BoolExpr -> Node -> Bool
--     filterFunc be n = fst $ boolReduction be ((trace "FILTER" nodeToEnvironment n): envs)

outOr :: Maybe Out -> Maybe Out -> Bool
(Just (Field (BoolField b1))) `outOr` (Just (Field (BoolField b2))) = b1 || b2
_ `outOr` _ = runtimeError "Invalid type for (||) operator"

outAnd :: Maybe Out -> Maybe Out -> Bool
(Just (Field (BoolField b1))) `outAnd` (Just (Field (BoolField b2))) = b1 && b2
_ `outAnd` _ = runtimeError "Invalid type for (&&) operator"

binaryExpressionReduction :: Expr -> Expr -> Environments -> (Maybe Out, Maybe Out, Environments)
binaryExpressionReduction e1 e2 envs = (o1, o2, envs2)
  where
    (o1, envs1) = exprReduction e1 envs --will error if type checker aint it
    (o2, envs2) = exprReduction e2 envs1  --will error if type checker aint it

processFile :: String -> IO()
processFile path = do
  file <- readFile path
  let tokens = Lexing.Tokens.alexScanTokens file
  -- putStrLn $ show tokens
  let ast = parser tokens
  -- putStrLn $ show ast
  let (GQLType varName filepath program) = ast
  n4jFile <- readFile filepath
  let n4jTokens = Lexing.N4jLexer.alexScanTokens n4jFile
  --putStrLn $ show n4jTokens
  --putStrLn $ show $ Graph $ parseToGraph n4jTokens
  let graph = parseToGraph n4jTokens
  -- putStrLn $ show graph

  --TYPE CHECK
  -- putStrLn "\nRunning:"
  let runProgram = run program [[(varName, Graph graph), ("null", Field NullField)]]
  -- print runProgram
  putStrLn $ seq runProgram ""

runtimeError :: String -> a
runtimeError s = error $ "Runtime Error: " ++ s

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> processFile inputFile
    _ -> do
      putStrLn "Usage: stack exec -- comp2212-coursework-exe <input-file>"

