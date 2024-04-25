module Interpreter (interpret) where

-- Importing necessary modules
import InputParser (parseInput, Tables, Table, Row(..), ID(..), Value(..), Labels, Relationship, Type(..), Types)
import Parser (Program, Statement(..), Expression(..), SetterExpression(..), MathExpr(..), BooleanExpression(..), Start(..), SettableExpression(..), Start)
import InputLexer (lexInput, Token(..))
import Printer (printOutput)

-- Environment
type Env = [(String, Data)] -- Variable name, Data

-- Continuation Stack
data Kont
    = KEmpty
    | KIf Program Program Env Kont 
    | KElif Program Program Program Env Kont
    | KFor  Program Env Kont
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
    | Reg String | Field (Type, Value)
    deriving (Eq, Show)

interpret :: Start -> Tables
interpret (StartExpr var file statements) = interpret' (statements, initialEnv, KEmpty)
    where
        fileData = getFile file
        initialEnv = [(var, G fileData)]

getFile :: String -> String
getFile file = do
    contents <- readFile file
    let fileData = parseInput $ lexInput contents
    return fileData

interpret' :: Control -> Control
interpret' ([], env, kont) = ([], env, kont)
interpret' (Print var:stmts, env, kont) = ([Print var], env, KEmpty)
interpret' (IfBlock boolx block:stmts, env, kont)
    | interpretBooleanExpression boolx env
        = interpret' (block++stmts, env, kont)
    | otherwise 
        = interpret' (stmts, env, kont)
interpret' (IfElseBlock boolx block1 block2:stmts, env, kont)
    | interpretBooleanExpression boolx env
        = interpret' (block1++stmts, env, kont)
    | otherwise
        = interpret' (block2++stmts, env, kont)
--TODO implement For block
interpret' (ForBlock varType var expr block) = undefined


interpret' (Expression (MathExpr mExpr):stmts, env, kont) = undefined
    where 
        result = interpretMathExpr mExpr
        field = IntValue result
interpret' (Expression (String str):stmts, env, kont) = undefined
    where 
        field = StringValue str
interpret' (Expression (Regex regex):stmts, env, kont) = undefined
    where 
        field = Reg regex
interpret' (Expression (MatchQuery str bExpr):stmts, env, kont) = G $ matchGraph g bExpr
    where
        (Just G g) = lookup str env
        matchTables str bExpr = map (matchRows bExpr) g
        -- matchTable bExpr table = map (matchRow bExpr header) rows
        matchRows bExpr table = filter (matchRow bExpr header) rows
            where 
                    header = head table
                    rows = tail table
        matchRow bExpr header row = interpretBooleanExpression bExpr newEnv
            where 
                row' = zip (getHeaderTypes header) (getRowValues row)
                newEnv = map nodeConvert row'
            
interpret' (Expression (AddQuery str expr):stmts, env, kont) = undefined
interpret' (Expression (BooleanExpression bExpr):stmts, env, kont) = undefined
interpret' (Expression (GetRelation str bExpr):stmts, env, kont) = undefined
interpret' (Expression (Exclude str expr):stmts, env, kont) = undefined
interpret' (Expression (GetNode str expt):stmts, env, kont) = undefined
interpret' (Expression (SetterExpression assExpr):stmts, env, kont) = undefined
interpret' (Expression (SettableExpression assignable):stmts, env, kont) = undefined

getHeaderTypes :: Row -> Types 
getHeaderTypes (Header types) = types
getHeaderTypes (LabeledHeader types) = types
getHeaderTypes (RelationshipHeader types) = types
getHeaderTypes _ = []

getRowValues :: Row -> [Value]
getRowValues (Data _ values) = values 
getRowValues (LabeledData _ values _) = values
getRowValues (RelationshipData _ values _ _) = values

nodeConvert :: (Type,Value) -> Env 
nodeConvert (t,v) = Field (t,v)

interpretBooleanExpression :: BooleanExpression -> Env -> Bool
interpretBooleanExpression (Bool True) = True
interpretBooleanExpression (Bool False) = False
interpretBooleanExpression (NotEquals expr1 expr2) = result1 \= result2
    where
        result1 = interpretExpr expr1
        result2 = interpretExpr expr2
interpretBooleanExpression (LessThan expr1 expr2) = result1 < result2
    where
        result1 = interpretExpr expr1
        result2 = interpretExpr expr2
interpretBooleanExpression (GreaterThan expr1 expr2) = result1 > result2
    where
        result1 = interpretExpr expr1
        result2 = interpretExpr expr2
interpretBooleanExpression (LTEquals expr1 expr2) = result1 <= result2
    where
        result1 = interpretExpr expr1
        result2 = interpretExpr expr2
interpretBooleanExpression (GTEquals expr1 expr2) = result1 >= result2
    where
        result1 = interpretExpr expr1
        result2 = interpretExpr expr2
interpretBooleanExpression (And expr1 expr2) = result1 && result2
    where
        result1 = interpretExpr expr1
        result2 = interpretExpr expr2
interpretBooleanExpression (Or expr1 expr2) = result1 || result2
    where
        result1 = interpretExpr expr1
        result2 = interpretExpr expr2
--TODO implement other BooleanExpressions interpretation
interpretBooleanExpression (EndRelationQuery bExpr str) = undefinded
    -- where
    --     case (lookup )
interpretBooleanExpression (StartRelationQuery str bExpr) = undefinded
interpretBooleanExpression (RelationQuery startStr bExpr endStr) = undefinded
interpretBooleanExpression (Contains expr strs) = undefinded

nodeReduction expr1 expr2 env = (out1, out2, env2)
    where 
        out1 = interpretExpr expr1 env
        out2 = interpretExpr expr2 env

interpretExpr :: Expression -> Data
interpretExpr (String str) = S str
interpretExpr (MathExpr mExpr) = I $ interpretMathExpr mExpr
interpretExpr (BooleanExpression bExpr) = B $ interpretBooleanExpression bExpr
--TODO implement other Exprs interpretation
interpretExpr (GetNode str expr) = N $ getNode str expr
interpretExpr (GetRelation str bExpr) = G $ getRelation str bExpr
interpretExpr (Regex regex) = S regex -- ?? 
interpretExpr (SettableExpression assignable) = undefined
interpretExpr (SetterExpression assExpr) = undefined
interpretExpr (Exclude str expr) = undefined
interpretExpr (AddQuery str expr) = undefined
interpretExpr (MatchQuery str bExpr) = undefined

interpretMathExpr :: MathExpr -> Int
interpretMathExpr (Int i) = i
interpretMathExpr (Addition expr1 expr2) = result1 + result2
    where
        result1 = interpretMathExpr expr1
        result2 = interpretMathExpr expr2
interpretMathExpr (Subtraction expr1 expr2) = result1 - result2
    where
        result1 = interpretMathExpr expr1
        result2 = interpretMathExpr expr2
interpretMathExpr (Multiplication expr1 expr2) = result1 * result2
    where
        result1 = interpretMathExpr expr1
        result2 = interpretMathExpr expr2
interpretMathExpr (Division expr1 expr2) = result1 `div` result2
    where
        result1 = interpretMathExpr expr1
        result2 = interpretMathExpr expr2

--TODO implement GetNode
getNode :: String -> Expression -> Row
getNode str expr = undefined

--TODO implement GetRelation
getRelation :: String -> BooleanExpression -> [Table]
getRelation str bExpr = undefined