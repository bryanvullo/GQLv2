module Interpreter (interpret) where

-- Importing necessary modules
import InputParser (parseInput, Tables, Table, Row(..), ID(..), Value(..), Labels, Relationship, Type(..), Types)
import Parser
import InputLexer (lexInput, Token(..))
import Printer (printOutput, printRow)
import GHC.Base (undefined)
import Data.IntMap (update)

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
data Data = G [Table] | N Row | B Bool | I Int | S String | Nil 
    | Reg String | Field (Class, Value)
    deriving (Eq, Show)

runtimeError :: String -> IO ()
runtimeError msg = putStrLn $ "Runtime Error: " ++ msg

getFile :: String -> IO Tables
getFile file = do
    contents <- readFile file
    let fileData = parseInput $ lexInput contents
    return fileData

interpret :: Start -> IO ()
interpret (StartExpr var file statements) = do 
    fileData <- getFile file
    let initialEnv = [(var, G fileData)]
    interpretProgram (statements, initialEnv)

interpretProgram :: (Program, Env) -> IO ()
interpretProgram ([], env) = return ()
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
interpretProgram (ThroughQuery varType var expr block:stmts, env) = do 
    let env' = runForBlock varType var expr block env
    interpretProgram (stmts, env')
interpretProgram (Expression statement:stmts, env) = do 
    let env' = interpretExpr (statement, env)
    interpretProgram (stmts, env')

handlePrint :: String -> Env -> IO ()
handlePrint var env = do
    case lookup var env of
            Just (G tables) -> printOutput tables
            Just (N row) -> printRow row
            Just (B bool) -> print bool
            Just (I int) -> print int
            Just (S str) -> print str
            Just (Reg regex) -> print regex
            _ -> runtimeError ("Variable " ++ var ++ " not found")

interpretExpr :: (Expression, Env) -> Env
interpretExpr (ExpressionLink sExpr, env) = interpretLink (sExpr, env)
--TODO Complete this function
interpretExpr (_, env) = env

interpretLink :: (ExpressionLink, Env) -> Env
--TODO Complete this function
interpretLink (Assign sExpr expr, env) = do 
    let value = interpretExprValue (expr, env)
    case sExpr of 
        Object x -> (x, value) : env
        ArgumentAttribute x y -> updateAttribute x y value env

interpretExprValue :: (Expression, Env) -> Data
interpretExprValue (String str, env) = S str
interpretExprValue (CaseQuery str bExpr, env) = undefined

--TODO below
updateAttribute :: String -> String -> Data -> Env -> Env
updateAttribute x y value env = undefined

interpretBoolValue :: ExpressionBool -> Env -> Bool
interpretBoolValue (Bool True) env = undefined

runForBlock :: Class -> String -> Expression -> Program -> Env -> Env
runForBlock varType var expr block env = undefined


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