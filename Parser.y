{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

-- Token definitions
%token
  int                         { Key (KeyNum $$)      _ }
  ACCESS                    { Key KeyACCESSToken        _ }
  CASE                       { Key KeyCASEToken           _ }
  STDOUT                       { Key KeySTDOUTToken           _ }
  'AND'                        { Key KeyLogicalAnd             _ }
  'OR'                        { Key KeyLogicalOr              _ }
  '('                         { Key KeyBracketLeft          _ }
  ')'                         { Key KeyBracketRight          _ }
  DataStructure                   { Key KeyDataStructureToken       _ }
  Num                 { Key KeyNumToken     _ }
  Chars                  { Key KeyCharsToken      _ }
  Bool                 { Key KeyBoolToken     _ }
  '{'                         { Key KeyBraceLeft          _ }
  '}'                         { Key KeyBraceRight          _ }
  CONDIF                          { Key KeyCONDIFToken              _ }
  CONDELIF                        { Key KeyCONDELIFToken            _ }
  THROUGH                         { Key KeyTHROUGHToken             _ }
  ':'                         { Key KeySeparatorColon           _ }
  identity                         { Key (KeyIdentity $$)     _ }
  '>='                        { Key KeyInequalitySlackGreater             _ }
  '<='                        { Key KeyInequalitySlackLesser             _ }
  '>'                         { Key KeyInequalityStrictGreater              _ }
  '<'                         { Key KeyInequalityStrictLesser              _ }
  '='                         { Key KeySet          _ }
  '=='                        { Key KeyIdentical          _ }
  '['                         { Key KeyBracketLeftSquare             _ }
  ']'                         { Key KeyBracketRightSquare             _ }
  '->'                        { Key KeyDirectionalRight          _ }
  '-'                         { Key KeyNumericMinus            _ }
  regular                       { Key (KeyRegularExpression $$)     _ }
  PLUS                         { Key KeyPlusToken             _ }
  '.'                         { Key KeyPeriod             _ }
  header                    { Key (KeyHeader $$)  _ }
  chars                      { Key (KeyChars $$)    _ }
  True                        { Key KeyBoolTrue       _ }
  False                       { Key KeyBoolFalse      _ }
  ';'                         { Key KeySeparatorColonSemi       _ }
  '!='                        { Key KeyIdenticalNot       _ }
  ','                         { Key KeySeparatorComma           _ }
  DataPoint                    { Key KeyDataPointToken        _ }
  Association                { Key KeyAssociationToken    _ }
  CALLASSOCIATION                 { Key KeyCallAssociationToken     _ }
  CONTAINS                    { Key KeyContainsToken        _ }
  CALLDATAPOINT                     { Key KeyCallDataPointToken         _ }
  '+'                         { Key KeyNumericAdd            _ }
  '*'                         { Key KeyNumericMultiply            _ }
  '/'                         { Key KeyNumericDivide          _ }
  '=+'                        { Key KeyNumericIncrease       _ }
  '=-'                        { Key KeyNumericDecrease       _ }
  NOT                     { Key KeyNotToken         _ }

-- Operator precedence
%right '=' '=+' '=-'
%left ','
%left ':'
%right 'OR'
%right 'AND'
%nonassoc '==' '!='
%nonassoc '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/'
%left '.'

%%

-- Grammar rules

-- Queries
Queries
  : Query Queries               { ($1 : $2) }
  | {- empty -}                     { [] }

-- Query
Query
  : Expr ';'                        { Expr $1 }
  | CONDIFQuery                     { $1 }
  | THROUGHQuery                    { $1 }

-- Expressions
Expr
  : ExtractExpr                     { $1 }
  | FuncExpr                        { $1 }
  | FuncAppExpr                     { $1 }
  | BoolExpr       %shift           { BoolExpr $1 }
  | SetExpr                      { $1 }
  | LiteralExpr                     { $1 }
  | '(' Expr ')'                    { $2 }

-- Literal Expressions
LiteralExpr
  : identity                             { Var $1 }
  | MathExpr                        { MathExpr $1 }
  | header                        { Var $1 }
  | chars                          { Chars $1 }
  | regular                           { RegularExpression $1 }

-- Setment Expressions
SetExpr
  : Type identity '=' Expr               { TypedSet $1 $2 $4 }
  | Expr '=' Expr                   { Set $1 $3 }
  | Type identity               %shift   { Declare $1 $2 }
  | Expr '=+' Expr                  { IncrementSet $1 $3 }
  | Expr '=-' Expr                  { DecrementSet $1 $3 }
  | Expr '.' CALLDATAPOINT '(' Expr ')'   { CallDataPoint $1 $5 }

-- Extract Expressions
ExtractExpr
  : Expr '.' identity                    { CallProperty $1 $3 }
  | Expr '.' header               { CallProperty $1 $3 }

-- Function Expressions
FuncExpr
  : ACCESS '(' chars ')'         { ACCESS $3 }
  | STDOUT '(' identity ')'               { STDOUT $3 }

-- Function Application Expressions
FuncAppExpr
  : Expr '.' CASE '(' BoolExpr ')'        { CASEQuery $1 $5 }
  | Expr '.' PLUS '(' Expr ')'              { PlusQuery $1 $5 }
  | Expr '.' CALLASSOCIATION '(' BoolExpr ')'  { CallAssociation $1 $5 }
  | Expr '.' NOT '(' Expr ')'          { Not $1 $5 }

-- Math Expressions
MathExpr
  : MathTerm                        { $1 }
  | MathExpr '+' MathExpr           { PlusPlus $1 $3 }
  | MathExpr '-' MathExpr           { Subtraction $1 $3 }

MathTerm
  : MathExpr '*' MathExpr           { Multiplication $1 $3 }
  | MathExpr '/' MathExpr           { Divison $1 $3 }
  | int                             { IntLit $1 }

-- Bool Expressions
BoolExpr
  : SimpleBoolExpr 'AND' BoolExpr    { And $1 $3 }
  | SimpleBoolExpr 'OR' BoolExpr    { Or $1 $3 }
  | SimpleBoolExpr     %shift       { $1 }

SimpleBoolExpr
  : True                            { BoolLit True }
  | False                           { BoolLit False }
  | Expr '==' Expr                  { Equals $1 $3 }
  | Expr '!=' Expr                  { NotEquals $1 $3 }
  | Expr '<' Expr                   { LessThan $1 $3 }
  | Expr '>' Expr                   { GreaterThan $1 $3 }
  | Expr '<=' Expr                  { LTEquals $1 $3 }
  | Expr '>=' Expr                  { GTEquals $1 $3 }
  | '-' '[' BoolExpr ']' '->' identity   { EndAssociationQuery $3 $6 }
  | identity '-' '[' BoolExpr ']' '->'   { StartAssociationQuery $1 $4 }
  | '(' BoolExpr ')'                { $2 }
  | identity '-' '[' BoolExpr ']' '->' identity { AssociationQuery $1 $4 $7 }
  | Expr '.' CONTAINS '(' CharsList ')'  { Contains $1 $5 }

-- If Query
CONDIFQuery
  : CONDIF '(' BoolExpr ')' '{' Queries '}'                         { CONDIFBlock $3 $6 }
  | CONDIF '(' BoolExpr ')' '{' Queries '}' CONDELIF '{' Queries '}'    { CONDELIFBlock $3 $6 $10 }

-- THROUGH Query
THROUGHQuery
  : THROUGH '(' Type identity ':' Expr ')' '{' Queries '}'         { THROUGHBlock $3 $4 $6 $9 }

-- Chars List
CharsList
  : chars                          { [$1] }
  | chars ',' CharsList           { ($1 : $3) }

-- Type
Type
  : DataStructure                       { TypeConstr $1 }
  | Num                     { TypeConstr $1 }
  | Chars                      { TypeConstr $1 }
  | Bool                     { TypeConstr $1 }
  | DataPoint                        { TypeConstr $1 }
  | Association                    { TypeConstr $1 }

{
parseError :: [Token] -> a
parseError [] = error "Parse error at end of input\n"
parseError (Key t (AlexPn _ x y) : _) = error $ "Error " ++ show t ++ ", see " ++ show x ++ ":" ++ show y ++ "\n"

type Queries
  = [Query]

data Query
  = Expr Expr
  | CONDIFBlock BoolExpr Queries
  | CONDELIFBlock BoolExpr Queries Queries
  | THROUGHBlock Type String Expr Queries
  deriving (Eq, Show)

data Expr
  = TypedSet Type String Expr
  | Set Expr Expr
  | Declare Type String
  | Var String
  | MathExpr MathExpr
  | Chars String
  | RegularExpression String
  | CASEQuery Expr BoolExpr
  | PlusQuery Expr Expr
  | ACCESS String
  | STDOUT String
  | BoolExpr BoolExpr
  | CallProperty Expr String
  | CallAssociation Expr BoolExpr
  | IncrementSet Expr Expr
  | DecrementSet Expr Expr
  | Not Expr Expr
  | CallDataPoint Expr Expr
  deriving (Eq, Show)

data MathExpr
  = IntLit Int
  | PlusPlus MathExpr MathExpr
  | Subtraction MathExpr MathExpr
  | Multiplication MathExpr MathExpr
  | Divison MathExpr MathExpr
  deriving (Eq, Show)

data BoolExpr
  = BoolLit Bool
  | Equals Expr Expr
  | NotEquals Expr Expr
  | LessThan Expr Expr
  | GreaterThan Expr Expr
  | LTEquals Expr Expr
  | GTEquals Expr Expr
  | And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  | EndAssociationQuery BoolExpr String
  | StartAssociationQuery String BoolExpr
  | AssociationQuery String BoolExpr String
  | Contains Expr [String]
  deriving (Eq, Show)

data Type
  = TypeConstr Token
  deriving (Eq, Show)
}