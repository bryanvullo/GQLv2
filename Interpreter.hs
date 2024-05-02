module Interpreter (interpret) where

-- Importing necessary modules
import InputParser (parseInput, Tables, Table, Row(..), ID(..), Value(..), Labels, Relationship, Type(..), Types)
import Parser
import InputLexer (lexInput, Token(..))
import Printer (printOutput, printRow, groupNodesToTables, printTables, GraphValue(..))
import GHC.Base (undefined)
import Data.Text.Array (run)
import Data.ByteString (find)


-- Environment
type Env = [(String, Data)] -- Variable name, Data

-- Continuation Stack
data Kont
    = KEmpty
    | KIf Program Program Env Kont 
    | KElif Program Program Program Env Kont
    | KFor  Program Env Kont
    | KSet ExpressionLink Env Kont
    deriving (Eq, Show)
    -- | KAssign String Env Kont
    -- | KSetL Expression Env Kont
    -- | KSetR String Env Kont
    -- | KNumericXX NumericXX Env Kont
    -- | KCase BoolXX Env Kont
    -- | KPlus Env Kont
    -- | KBoolXX BoolXX Env Kont
    -- | KAttr String Env Kont
    -- | KAssoc BoolXX Env Kont
    -- | KIncrease Env Kont
    -- | KDecrease Env Kont
    -- | KNot Env Kont
    -- | KDataPoint BoolXX Env Kont
    -- | KCondIf BoolXX Program Program Env Kont
    -- | KCondElseIf BoolXX Program Program Env Kont
    -- | KThrough Class String Expression Program Env Kont
    -- | KSeq Program Env Kont

-- Control State
type Control = (Program, Env, Kont) -- Statements, Environment, Continuation

-- Data Type
data Data = G Graph | N Node | V GraphValue
    | Reg String | Field (Class, Value)
    deriving (Eq, Show)

runtimeError :: String -> a
runtimeError msg = error $ "Runtime Error: " ++ msg

getFile :: String -> IO Tables
getFile file = do
    contents <- readFile file
    let fileData = parseInput $ lexInput contents
    return fileData

instance Ord GraphValue where
    compare (S s1) (S s2) = compare s1 s2
    compare (I i1) (I i2) = compare i1 i2
    compare (B b1) (B b2) = compare b1 b2
    compare Null Null = EQ
    compare Null _ = LT
    compare _ Null = GT
    compare _ _ = runtimeError "Unsupported comparison"

type Node = [(String, String, GraphValue)]
type Graph = [Node]

lookupNode :: String -> Node -> Maybe GraphValue
lookupNode attr node 
    | null triples = Nothing
    | otherwise = Just $ (\(_,_,v) -> v) $ head triples
    where 
        triples = filter (\(x,_,_) -> x == attr) node

tablesToGraph :: [Table] -> Graph 
tablesToGraph tables = concatMap tableToGraph tables

tableToGraph :: Table -> [Node]
tableToGraph table = map (rowToNode header) rows
    where 
        header = head table
        rows = tail table

rowToNode :: Row -> Row -> Node
rowToNode (Header types) (Data (Id id) values) = 
    ("ID","", ID id) : nodeAttributes
    where
        typeNames = map getTypeName types
        graphValues = map valueToGraphValue values
        nodeAttributes = map (\((tn,t),v) -> (tn, t, v)) $ zip typeNames graphValues
rowToNode (LabeledHeader types) (LabeledData (Id id) values labels) = 
    ("ID","", ID id) : nodeAttributes ++ [("LABEL","", Ss labels)]
    where
        typeNames = map getTypeName types
        graphValues = map valueToGraphValue values
        nodeAttributes = map (\((tn,t),v) -> (tn, t, v)) $ zip typeNames graphValues
rowToNode (RelationshipHeader types) 
    (RelationshipData (Id start) values (Id end) relationship) = 
    ("START_ID","", ID start) : nodeAttributes ++ [("END_ID","", ID end), ("TYPE","", ID relationship)]
    where
        typeNames = map getTypeName types
        graphValues = map valueToGraphValue values
        nodeAttributes = map (\((tn,t),v) -> (tn, t, v)) $ zip typeNames graphValues
rowToNode _ _ = runtimeError "Parsing Error on Input Data (n4j file), invalid row type." 

getTypeName :: Type -> (String, String)
getTypeName (StringType name) = (name, "string")
getTypeName (IntType name) = (name, "integer")
getTypeName (BoolType name) = (name, "boolean")

valueToGraphValue :: Value -> GraphValue
valueToGraphValue (StringValue str) = S str
valueToGraphValue (IntValue i) = I i
valueToGraphValue (BoolValue b) = B b
valueToGraphValue NullValue = Null

interpret :: Start -> IO ()
interpret (StartExpr var file statements) = do 
    fileData <- getFile file
    let graph = tablesToGraph fileData
    let initialEnv = [(var, G graph)]
    env <- interpretProgram (statements, initialEnv)
    return ()

interpretProgram :: (Program, Env) -> IO (Env)
interpretProgram ([], env) = return env
interpretProgram (Output var:stmts, env) = do 
    handlePrint var env
    interpretProgram (stmts, env)
interpretProgram (CondIfQuery boolx block:stmts, env) 
    | interpretBoolValue boolx env = do 
        interpretProgram (block ++ stmts, env)
    | otherwise = interpretProgram (stmts, env)
interpretProgram (CondElifQuery boolx block1 block2:stmts, env) 
    | interpretBoolValue boolx env = do 
        interpretProgram (block1 ++ stmts, env)
    | otherwise = do 
        interpretProgram (block2 ++ stmts, env)
interpretProgram (ThroughQuery varType var vars block:stmts, env) = do 
    env' <- runThroughBlock varType var vars block env
    interpretProgram (stmts, env')
interpretProgram (Expression statement:stmts, env) = do 
    let env' = interpretExpr (statement, env)
    interpretProgram (stmts, env')

handlePrint :: String -> Env -> IO ()
handlePrint var env = do
    case lookup var env of
        -- Just (G graph) -> print env
        Just (G graph) -> printTables graph
        Just (N node) -> print node
        Just (V (I i)) -> print i
        Just (V (S str)) -> print str
        Just (V (B b)) -> print b
        Just (V Null) -> putStrLn "Null"
        Just (Reg regex) -> print regex
        _ -> runtimeError ("Variable " ++ var ++ " not found")

updateEnv :: String -> Data -> Env -> Env
updateEnv var value env = case lookup var env of 
    Just _ -> map (\x -> if fst x == var then (var, value) else x) env --update
    Nothing -> (var, value) : env --add

interpretExpr :: (Expression, Env) -> Env
interpretExpr (ExpressionLink sExpr, env) = interpretLink (sExpr, env)
interpretExpr (RemoveQuery graphName nodeName, env) = updateEnv graphName (G graph') env
    where
        graph = case lookup graphName env of 
            Just (G g) -> g
            Just _ -> runtimeError ("Variable " ++ graphName ++ " is not a graph! unable to remove node from it.")
            _ -> runtimeError ("Variable " ++ graphName ++ " not found")
        node = case lookup nodeName env of 
            Just (N n) -> n
            Just _ -> runtimeError ("Variable " ++ nodeName ++ " is not a node! unable to add it to graph.")
            _ -> runtimeError ("Variable " ++ nodeName ++ " not found")
        graph' = filter ( == node) graph
interpretExpr (AddQuery graphName nodeName, env) = updateEnv graphName (G graph'') env
    where
        graph = case lookup graphName env of 
            Just (G g) -> g
            Just _ -> runtimeError ("Variable " ++ graphName ++ " is not a graph! unable to add node to it.")
            _ -> runtimeError ("Variable " ++ graphName ++ " not found")
        node = case lookup nodeName env of 
            Just (N n) -> n
            Just _ -> runtimeError ("Variable " ++ nodeName ++ " is not a node! unable to add it to graph.")
            _ -> runtimeError ("Variable " ++ nodeName ++ " not found")
        graph' = filter (not . hasSameNodeSignature node) graph
        graph'' = node : graph'
interpretExpr (_, env) = env

hasSameNodeSignature :: Node -> Node -> Bool
hasSameNodeSignature node1 node2 
    | any (\(x,_,_) -> x == "ID") node1 = case (lookupNode "ID" node1, lookupNode "ID" node2) of 
        (Just (ID id1), Just (ID id2)) -> id1 == id2
        _ -> False
    | any (\(x,_,_) -> x == "TYPE") node1 = case 
        (lookupNode "START_ID" node1, lookupNode "END_ID" node1, lookupNode "TYPE" node1,
         lookupNode "START_ID" node2, lookupNode "END_ID" node2, lookupNode "TYPE" node2) of
            (Just (ID start1), Just (ID end1), Just (ID type1), Just (ID start2), Just (ID end2), Just (ID type2)) -> 
                (start1 == start2) && (end1 == end2) && (type1 == type2) 
            _ -> False
    | otherwise = False


interpretLink :: (ExpressionLink, Env) -> Env
interpretLink (Assign sExpr expr, env) = do 
    let value = interpretExprValue (expr, env)
    case sExpr of 
        Object x -> updateEnv x value env 
        ArgumentAttribute x y -> updateAttribute x y value env
interpretLink (Assert varType var, env) = case varType of 
    GraphClass -> (var, G []) : env
    NodeClass -> (var, N []) : env
    RelationClass -> (var, N []) : env
    IntegerClass -> (var, V (I 0)) : env
    _ -> (var, V Null) : env
interpretLink (ClassArgumentStatement varType var expr, env) = updateEnv var value env
    where
        value = interpretExprValue (expr, env)
interpretLink _ = runtimeError "Unsupported Expression Link Operation"

interpretExprValue :: (Expression, Env) -> Data
interpretExprValue (String str, env) = V (S str)
interpretExprValue (ExpressionMathXAS (Num x), env) = V (I x)
interpretExprValue (ExpressionBool (Bool b), env) = V (B b)
interpretExprValue (CaseQuery str bExpr, env) =
    case lookup str env of 
        Just (G graph) -> G (caseGraph bExpr graph)
        Just (N node) -> if caseNode bExpr node then N node else V Null
        _ -> runtimeError ("Unable to use CASE on variable " ++ str)
    where 
        caseGraph bExpr graph = filter (caseNode bExpr) graph
interpretExprValue (NegateData str bExpr, env) = 
        case lookup str env of 
        Just (G graph) -> G (negateGraph bExpr graph)
        Just (N node) -> if caseNode bExpr node then N node else V Null
        _ -> runtimeError ("Unable to use CASE on variable " ++ str)
    where 
        negateGraph bExpr graph = filter (\x -> not (caseNode bExpr x)) graph
interpretExprValue (ArgumentConstructor (ArgumentAttribute node attr), env) = 
    case lookup node env of 
        Just (N node) -> case lookupNode attr node of 
            Just value -> V value
            Nothing -> V Null
        Just x -> runtimeError ("Variable " ++ node ++ " is not a node! cannot access attribute " ++ attr)
        _ -> runtimeError ("Variable " ++ node ++ " not found when accessing attribute " ++ attr)
interpretExprValue (ArgumentConstructor (Object "null"), _) = V Null
interpretExprValue (ArgumentConstructor (Object x), env) = case lookup x env of 
    Just (N node) -> N node
    Just (G graph) -> G graph
    Just (V value) -> V value
    _ -> runtimeError ("Variable " ++ x ++ " not found")
interpretExprValue (AssociationQuery str e@(AssociationEnd _ _ _), env) = N assoc
    where 
        id = case lookup str env of 
            Just (N node) -> case lookupNode "ID" node of 
                Just (ID id) -> id
                _ -> runtimeError ("Node " ++ str ++ " does not have an ID attribute")
            _ -> runtimeError ("Variable " ++ str ++ " not found")
        assoc = head $ filter (isStartOfAssoc id) assocs
        assocs = getAssocs e env 
interpretExprValue (AssociationQuery str e@(AssociationStart _ _ _), env) = N assoc
    where 
        id = case lookup str env of 
            Just (N node) -> case lookupNode "ID" node of 
                Just (ID id) -> id
                _ -> runtimeError ("Node " ++ str ++ " does not have an ID attribute")
            _ -> runtimeError ("Variable " ++ str ++ " not found")
        assoc = head $ filter (isEndOfAssoc id) assocs
        assocs = getAssocs e env
interpretExprValue (ExpressionLink (ArgumentIncrement arg expr), env) = V (I (v1 + v2))
    where 
        v1 = case arg of 
            Object x -> case lookup x env of 
                Just (V (I i)) -> i
                _ -> runtimeError "Increment Value is not an Integer"
            ArgumentAttribute x y -> case getNodeAttribute y node of 
                I i -> i
                _ -> runtimeError "Increment Value is not an Integer"
                where 
                    node = case lookup x env of 
                        Just (N n) -> n
                        _ -> runtimeError ("Variable " ++ x ++ " is not a node!")
        v2 = case interpretExprValue (expr, env) of 
            V (I i) -> i
            _ -> runtimeError "Increment Value is not an Integer"
interpretExprValue (x, env) = runtimeError ("Unsupported Expression Value Reduction " ++ show x ++ show env)

getNodeAttribute :: String -> Node -> GraphValue
getNodeAttribute attr node = case lookupNode attr node of 
    Just value -> value
    Nothing -> Null

getAssocs :: ExpressionBool -> Env -> [Node]
getAssocs (AssociationEnd bExpr str valueStr) env = findEndAssocs graph node
    where 
        graph = case lookup valueStr env of 
            Just (G g) -> g
            Just _ -> runtimeError ("Variable " ++ valueStr ++ " is not a graph!")
            _ -> runtimeError ("Variable " ++ valueStr ++ " not found")
        node = case lookup str env of 
            Just (N n) -> n
            Just _ -> runtimeError ("Variable " ++ str ++ " is not a node!")
            _ -> runtimeError ("Variable " ++ str ++ " not found")
getAssocs (AssociationStart str bExpr valueStr) env = findStartAssocs graph node
    where 
        graph = case lookup valueStr env of 
            Just (G g) -> g
            Just _ -> runtimeError ("Variable " ++ valueStr ++ " is not a graph!")
            _ -> runtimeError ("Variable " ++ valueStr ++ " not found")
        node = case lookup str env of 
            Just (N n) -> n
            Just _ -> runtimeError ("Variable " ++ str ++ " is not a node!")
            _ -> runtimeError ("Variable " ++ str ++ " not found")
getAssocs _ _ = runtimeError "Unsupported Association Query"

caseNode :: ExpressionBool -> Node -> Bool
caseNode (BoolUnion expr1 expr2) node = caseNode expr1 node || caseNode expr2 node
caseNode (BoolConjunction expr1 expr2) node = caseNode expr1 node && caseNode expr2 node
caseNode (StrictEqualityQuery expr1 expr2) node 
    | r1 == Null || r2 == Null = False
    | otherwise = r1 == r2
    where 
        r1 = getNodeValueComparison expr1 node
        r2 = getNodeValueComparison expr2 node
caseNode (SlackLesserQuery expr1 expr2) node
    | r1 == Null || r2 == Null = False
    | otherwise = r1 < r2
    where 
        r1 = getNodeValueComparison expr1 node
        r2 = getNodeValueComparison expr2 node
caseNode (HasQuery expr1 label) node 
    | r == Null = False
    | otherwise = label `elem` labels
    where 
        r = getNodeValueComparison expr1 node
        labels = case r of 
            Ss labels -> labels
            _ -> []
caseNode _ _ = runtimeError "Unsupported Boolean Operation on Node"

getNodeValueComparison :: Expression -> Node -> GraphValue
getNodeValueComparison (ExpressionMathXAS (Num x)) _ = I x 
getNodeValueComparison (String str) _ = S str
getNodeValueComparison (ArgumentConstructor (Object x)) node = 
    case lookupNode x node of 
        Just (ID id) -> S id
        Just value -> value
        Nothing -> Null
getNodeValueComparison _ _ = runtimeError "Unsupported Node Value"

--TODO below 
updateAttribute :: String -> String -> Data -> Env -> Env
updateAttribute nodeName attr value env = updateEnv nodeName (N node') env
    where 
        node' = case lookup nodeName env of
            Just (N node) -> updateAttribute' node attr value env
            Just _ -> runtimeError ("Variable " ++ nodeName ++ " is not a node!")
            _ -> runtimeError ("Variable " ++ nodeName ++ " not found")

updateAttribute' :: Node -> String -> Data -> Env -> Node
updateAttribute' node attr value env = node'
    where 
        (nodeValue,t) = case value of 
            V (S str) -> (S str, "string")
            V (ID str) -> (ID str, "")
            V (Ss strs) -> (Ss strs, "")
            V (I i) -> (I i, "integer")
            V (B b) -> (B b, "boolean")
            V Null -> (Null, "")
            x -> runtimeError ("Unsupported Value Type" ++ show x)
        node' = case lookupNode attr node of
            Just _ -> map (\(x,y,z) -> if x == attr then (x,y,nodeValue) else (x,y,z)) node --update
            Nothing -> (attr,t,nodeValue) : node --add
        
interpretBoolValue :: ExpressionBool -> Env -> Bool
interpretBoolValue (Bool True) env = undefined
interpretBoolValue (AssociationEnd bExpr str valueStr) env = interpretPredicateOnAssociation bExpr assocs env
    where 
        graph = case lookup valueStr env of 
            Just (G g) -> g
            Just _ -> runtimeError ("Variable " ++ valueStr ++ " is not a graph!")
            _ -> runtimeError ("Variable " ++ valueStr ++ " not found")
        node = case lookup str env of 
            Just (N n) -> n
            Just _ -> runtimeError ("Variable " ++ str ++ " is not a node!")
            _ -> runtimeError ("Variable " ++ str ++ " not found")
        assocs = findEndAssocs graph node
interpretBoolValue (AssociationStart str bExpr valueStr) env = interpretPredicateOnAssociation bExpr assocs env
    where 
        graph = case lookup valueStr env of 
            Just (G g) -> g
            Just _ -> runtimeError ("Variable " ++ valueStr ++ " is not a graph!")
            _ -> runtimeError ("Variable " ++ valueStr ++ " not found")
        node = case lookup str env of 
            Just (N n) -> n
            Just _ -> runtimeError ("Variable " ++ str ++ " is not a node!")
            _ -> runtimeError ("Variable " ++ str ++ " not found")
        assocs = findStartAssocs graph node
interpretBoolValue (AssociationStatement str1 bExpr str2 valueStr) env = interpretPredicateOnAssociation bExpr assocs env
    where 
        graph = case lookup valueStr env of 
            Just (G g) -> g
            Just _ -> runtimeError ("Variable " ++ valueStr ++ " is not a graph!")
            _ -> runtimeError ("Variable " ++ valueStr ++ " not found")
        node1 = case lookup str1 env of 
            Just (N n) -> n
            Just _ -> runtimeError ("Variable " ++ str1 ++ " is not a node!")
            _ -> runtimeError ("Variable " ++ str1 ++ " not found")
        node2 = case lookup str2 env of 
            Just (N n) -> n
            Just _ -> runtimeError ("Variable " ++ str2 ++ " is not a node!")
            _ -> runtimeError ("Variable " ++ str2 ++ " not found")
        assocs = findAssocs graph node1 node2
interpretBoolValue (BoolConjunction b1 b2) env = interpretBoolValue b1 env && interpretBoolValue b2 env
interpretBoolValue (BoolUnion b1 b2) env = interpretBoolValue b1 env || interpretBoolValue b2 env
interpretBoolValue (StrictInqualityQuery expr1 expr2) env = result1 /= result2
    where 
        result1 = interpretExprValue (expr1, env)
        result2 = interpretExprValue (expr2, env)
interpretBoolValue (StrictEqualityQuery expr1 expr2) env = result1 == result2
    where 
        result1 = interpretExprValue (expr1, env)
        result2 = interpretExprValue (expr2, env)
interpretBoolValue b _ = runtimeError ("Unsupported Boolean Value Reduction " ++ show b)

interpretPredicateOnAssociation :: ExpressionBool -> [Node] -> Env -> Bool
interpretPredicateOnAssociation (SlackGreaterQuery (ArgumentConstructor (Object x)) (ExpressionMathXAS (Num n))) assocs env = 
    any (\node -> case lookupNode x node of 
        Just (I i) -> i >= n
        _ -> False) assocs
interpretPredicateOnAssociation (StrictGreaterQuery (ArgumentConstructor (Object x)) (ExpressionMathXAS (Num n))) assocs env = 
    any (\node -> case lookupNode x node of 
        Just (I i) -> i > n
        _ -> False) assocs
interpretPredicateOnAssociation (SlackLesserQuery (ArgumentConstructor (Object x)) (ExpressionMathXAS (Num n))) assocs env = 
    any (\node -> case lookupNode x node of 
        Just (I i) -> i <= n
        _ -> False) assocs
interpretPredicateOnAssociation (StrictLesserQuery (ArgumentConstructor (Object x)) (ExpressionMathXAS (Num n))) assocs env = 
    any (\node -> case lookupNode x node of 
        Just (I i) -> i < n
        _ -> False) assocs
interpretPredicateOnAssociation (StrictEqualityQuery (ArgumentConstructor (Object x)) exprToCompare) assocs env = 
    any (\node -> case lookupNode x node of
        Just (ID id) -> V (S id) == valueToCompare
        Just value -> V value == valueToCompare
        _ -> False) assocs
    where 
        valueToCompare = interpretExprValue (exprToCompare, env)
interpretPredicateOnAssociation _ _ _ = runtimeError "Unsupported Predicate on Association"

findAssocs :: Graph -> Node -> Node -> [Node]
findAssocs graph startNode endNode = filter (isAssoc startID endID) graph
    where 
        startID = case lookupNode "ID" startNode of 
            Just (ID id) -> id
            _ -> ""
        endID = case lookupNode "ID" endNode of 
            Just (ID id) -> id
            _ -> ""

isAssoc :: String -> String -> Node -> Bool
isAssoc startID endID node = case (lookupNode "START_ID" node, lookupNode "END_ID" node) of 
    (Just (ID sID), Just (ID eID)) -> (sID == startID) && (eID == endID)
    _ -> False

findStartAssocs :: Graph -> Node -> [Node]
findStartAssocs graph node = filter (isStartOfAssoc nodeID) graph
    where 
        nodeID = case lookupNode "ID" node of 
            Just (ID id) -> id
            _ -> ""

isStartOfAssoc :: String -> Node -> Bool
isStartOfAssoc id node = case lookupNode "START_ID" node of 
    Just (ID startID) -> startID == id
    _ -> False

findEndAssocs :: Graph -> Node -> [Node]
findEndAssocs graph node = filter (isEndOfAssoc nodeID) graph
    where 
        nodeID = case lookupNode "ID" node of 
            Just (ID id) -> id
            _ -> ""

isEndOfAssoc :: String -> Node -> Bool
isEndOfAssoc id node = case lookupNode "END_ID" node of 
    Just (ID endID) -> endID == id
    _ -> False

runThroughBlock :: Class -> String -> String -> Program -> Env -> IO Env
runThroughBlock varType var vars block env = runThroughBlock' var nodes block env
    where 
        nodes = case lookup vars env of 
            Just (G xs) -> xs
            Nothing -> runtimeError ("Variable " ++ vars ++ " not found")
            _ -> runtimeError ("Variable " ++ vars ++ " is not a graph! cannot iterate over it")

runThroughBlock' :: String -> [Node] -> Program -> Env -> IO Env
runThroughBlock' var [] block env = return env
runThroughBlock' var (node:nodes) block env = do 
    -- let rmvPrevNodeEnv = filter (\x -> fst x /= var) env
    -- let env' = (var, N node) : rmvPrevNodeEnv
    let env' = updateEnv var (N node) env
    env'' <- interpretProgram (block, env')
    runThroughBlock' var nodes block env''

-- interpret' :: Control -> Control
-- interpret' ([], env, kont) = ([], env, kont)
-- interpret' (Print var:stmts, env, kont) = ([Print var], env, KEmpty)
-- interpret' (IfBlock boolx block:stmts, env, kont)
--     | interpretBooleanExpression boolx env
--         = interpret' (block++stmts, env, kont)
--     | otherwise 
--         = interpret' (stmts, env, kont)
-- interpret' (IfElseBlock boolx block1 block2:stmts, env, kont)
--     | interpretBooleanExpression boolx env
--         = interpret' (block1++stmts, env, kont)
--     | otherwise
--         = interpret' (block2++stmts, env, kont)
-- --TODO implement For block
-- interpret' (ForBlock varType var expr block:stmts, env, kont) = undefined


-- interpret' (Expression (NumExpression mExpr):stmts, env, kont) = undefined
--     where 
--         result = interpretMathExpr mExpr env
--         field = IntValue result
-- interpret' (Expression (String str):stmts, env, kont) = undefined
--     where 
--         field = StringValue str
-- interpret' (Expression (Regex regex):stmts, env, kont) = undefined
--     where 
--         field = Reg regex
-- interpret' (Expression (CaseQuery str bExpr):stmts, env, KSet sExpr env' kont) = undefined
--     where
--         graph = G $ matchTables g bExpr
--         set = interpretSExpr sExpr env' kont

--         Just (G g) = lookup str env
--         matchTables str bExpr = map (matchRows bExpr) g
--         -- matchTable bExpr table = map (matchRow bExpr header) rows
--         matchRows bExpr table = filter (matchRow bExpr header) rows
--             where 
--                     header = head table
--                     rows = tail table
--         matchRow bExpr header row = interpretBooleanExpression bExpr newEnv
--             where 
--                 row' = zip (getHeaderTypes header) (getRowValues row)
--                 newEnv = concatMap nodeConvert row'
            
-- interpret' (Expression (AddQuery str expr):stmts, env, kont) = undefined
-- interpret' (Expression (BooleanExpression bExpr):stmts, env, kont) = undefined
-- interpret' (Expression (GetRelation str bExpr):stmts, env, kont) = undefined
-- interpret' (Expression (Exclude str expr):stmts, env, kont) = undefined
-- interpret' (Expression (GetNode str expt):stmts, env, kont) = undefined
-- interpret' (Expression (SetterExpression assExpr):stmts, env, kont) = 
--     interpret' (stmts, env, kont)
--     where 
--             (expr, env', kont') = interpretSetter assExpr env kont 

-- interpret' (Expression (SettableExpression assignable):stmts, env, kont) = undefined

-- interpretSetter :: SetterExpression -> Env -> Kont -> (Expression, Env, Kont)
-- interpretSetter (TypedSet t str expr) env kont = undefined
-- interpretSetter (IncrSet sExpr expr) env kont = undefined
-- interpretSetter (DecrSet sExpr expr) env kont = undefined
-- interpretSetter (Set sExpr expr) env kont = (expr, env, KSet sExpr env kont)
-- interpretSetter (Declare t str) env kont = undefined

-- interpretSExpr :: SetterExpression -> Env -> Kont -> Env    
-- interpretSExpr = undefined

-- getHeaderTypes :: Row -> Types 
-- getHeaderTypes (Header types) = types
-- getHeaderTypes (LabeledHeader types) = types
-- getHeaderTypes (RelationshipHeader types) = types
-- getHeaderTypes _ = []

-- getRowValues :: Row -> [Value]
-- getRowValues (Data _ values) = values 
-- getRowValues (LabeledData _ values _) = values
-- getRowValues (RelationshipData _ values _ _) = values

-- nodeConvert :: (Type,Value) -> Env 
-- nodeConvert (t,v) = Field (t,v)

-- interpretBooleanExpression :: BooleanExpression -> Env -> Bool
-- interpretBooleanExpression (Bool True) env = True
-- interpretBooleanExpression (Bool False) env = False
-- interpretBooleanExpression (NotEquals expr1 expr2) env = result1 \= result2
--     where
--         result1 = interpretExpr expr1 env
--         result2 = interpretExpr expr2 env
-- interpretBooleanExpression (LessThan expr1 expr2 env) = result1 < result2
--     where
--         result1 = interpretExpr expr1 env
--         result2 = interpretExpr expr2 env
-- interpretBooleanExpression (GreaterThan expr1 expr2) env = result1 > result2
--     where
--         result1 = interpretExpr expr1 env
--         result2 = interpretExpr expr2 env
-- interpretBooleanExpression (LTEquals expr1 expr2) env = result1 <= result2
--     where
--         result1 = interpretExpr expr1 env
--         result2 = interpretExpr expr2 env
-- interpretBooleanExpression (GTEquals expr1 expr2) env = result1 >= result2
--     where
--         result1 = interpretExpr expr1 env
--         result2 = interpretExpr expr2 env
-- interpretBooleanExpression (And expr1 expr2) env = result1 && result2
--     where
--         result1 = interpretExpr expr1 env
--         result2 = interpretExpr expr2 env
-- interpretBooleanExpression (Or expr1 expr2) env = result1 || result2
--     where
--         result1 = interpretExpr expr1 env
--         result2 = interpretExpr expr2 env
-- --TODO implement other BooleanExpressions interpretation
-- interpretBooleanExpression (EndRelationQuery bExpr str) env = undefinded
--     -- where
--     --     case (lookup )
-- interpretBooleanExpression (StartRelationQuery str bExpr) env = undefinded
-- interpretBooleanExpression (RelationQuery startStr bExpr endStr) env = undefinded
-- interpretBooleanExpression (Contains expr strs) env = undefinded

-- nodeReduction expr1 expr2 env = (out1, out2, env2)
--     where 
--         out1 = interpretExpr expr1 env
--         out2 = interpretExpr expr2 env

-- interpretExpr :: Expression -> Env -> (Data, Env)
-- interpretExpr (String str) env = (S str, env)
-- interpretExpr (NumExpression mExpr) env = (I $ interpretMathExpr mExpr, env)
-- interpretExpr (BooleanExpression bExpr) env = (B $ interpretBooleanExpression bExpr, env)
-- --TODO implement other Exprs interpretation
-- interpretExpr (GetNode str expr) env = (N $ getNode str expr, env)
-- interpretExpr (GetRelation str bExpr) env = (G $ getRelation str bExpr, env)
-- interpretExpr (Regex regex) env = (S regex, env) -- ?? 
-- interpretExpr (SettableExpression assignable) env = undefined
-- interpretExpr (SetterExpression assExpr) env = undefined
-- interpretExpr (Exclude str expr) env = undefined
-- interpretExpr (AddQuery str expr) env = undefined
-- interpretExpr (CaseQuery str bExpr) env = undefined

-- interpretMathExpr :: NumExpression -> Env -> Int
-- interpretMathExpr (Int i) env = i
-- interpretMathExpr (Addition expr1 expr2) env = result1 + result2
--     where
--         result1 = interpretMathExpr expr1 env
--         result2 = interpretMathExpr expr2 env
-- interpretMathExpr (Subtraction expr1 expr2 env) = result1 - result2
--     where
--         result1 = interpretMathExpr expr1 env
--         result2 = interpretMathExpr expr2 env
-- interpretMathExpr (Multiplication expr1 expr2) env = result1 * result2
--     where
--         result1 = interpretMathExpr expr1 env
--         result2 = interpretMathExpr expr2 env
-- interpretMathExpr (Division expr1 expr2) env = result1 `div` result2
--     where
--         result1 = interpretMathExpr expr1 env
--         result2 = interpretMathExpr expr2 env

-- --TODO implement GetNode
-- getNode :: String -> Expression -> Row
-- getNode str expr = undefined

-- --TODO implement GetRelation
-- getRelation :: String -> BooleanExpression -> [Table]
-- getRelation str bExpr = undefined