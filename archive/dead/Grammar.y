{
module Parsing.Grammar where
import Lexing.Tokens
}

%name parser
%tokentype { Token }

%error { parseError }

%token
  int                         { Tn (TokenInt $$)       _ }
  READFILE                    { Tn (TokenReadFile)     _ }
  MATCH                       { Tn (TokenMatch)        _ }
  PRINT                       { Tn (TokenPrint)        _ }
  '"'                         { Tn (TokenDelimiter)    _ }
  '&&'                        { Tn (TokenAnd)          _ }
  '||'                        { Tn (TokenOr)           _ }
  '('                         { Tn (TokenLParen)       _ }
  ')'                         { Tn (TokenRParen)       _ }
  GraphType                   { Tn (TokenGraphType)    _ }
  IntegerType                 { Tn (TokenIntegerType)  _ }
  StringType                  { Tn (TokenStringType)   _ }
  BooleanType                 { Tn (TokenBooleanType)  _ }
  '{'                         { Tn (TokenLCurl)        _ }
  '}'                         { Tn (TokenRCurl)        _ }
  IF                          { Tn (TokenIf)           _ }
  ELSE                        { Tn (TokenElse)         _ }
  FOR                         { Tn (TokenFor)          _ }
  ':'                         { Tn (TokenColon)        _ }
  var                         { Tn (TokenVar $$)       _ }
  '>='                        { Tn (TokenGEQ)          _ }
  '<='                        { Tn (TokenLEQ)          _ }
  '>'                         { Tn (TokenGT)           _ }
  '<'                         { Tn (TokenLT)           _ }
  '='                         { Tn (TokenAssign)       _ }
  '=='                        { Tn (TokenEquals)       _ }
  '['                         { Tn (TokenLSQ)          _ }
  ']'                         { Tn (TokenRSQ)          _ }
  '->'                        { Tn (TokenRArrow)       _ }
  '-'                         { Tn (TokenDash)         _ }
  regex                       { Tn (TokenRegex $$)     _ }
  ADD                         { Tn (TokenAdd)          _ }
  '.'                         { Tn (TokenDot)          _ }
  bigField                    { Tn (TokenBigField $$)  _ }
  string                      { Tn (TokenString $$)    _ }
  True                        { Tn (TokenTrue)         _ }
  False                       { Tn (TokenFalse)        _ }
  ';'                         { Tn (TokenSemicolon)    _ }
  '!='                        { Tn (TokenNotEquals)    _ }
  ','                         { Tn (TokenComma)        _ }
  NodeType                    { Tn (TokenNodeType)     _ }
  RelationType                { Tn (TokenRelationType) _ }

%right '||'
%right '&&'

%%

Program
  : Statement                       { [$1] }
  | Statement Program               { ($1 : $2) }

Statement
  : Expr ';'                        { Expr $1 }
  | IfStatement                     { $1 }
  | ForStatement                    { $1 }

Expr
  : Type var '=' Expr               { TypedAssign $1 $2 $4 }
  | var '=' Expr                    { Assign $1 $3 }
  | Type var                        { Declare $1 $2 }
  | var                             { Var $1 }
  | int                             { Int $1 }
  | bigField                        { Var $1 }
  | string                          { String $1 }
  | var '.' MATCH '(' BoolExpr ')'  { MatchQuery $1 $5 }
  | var '.' ADD '(' NewNode ')'     { AddQuery $1 $5 }
  | READFILE string                 { ReadFile $2 }
  | PRINT '(' var ')'               { Print $3 }
  | BoolExpr                        { BoolExpr $1 }
  | var '.' var                     { GetProperty $1 $3 }
  | var '.' bigField                { GetProperty $1 $3 }

BoolExpr
  : True                              { Bool True }
  | False                             { Bool False }
  | Expr '==' Expr                    { Equals $1 $3 }
  | Expr '!=' Expr                    { NotEquals $1 $3}
  | Expr '<' Expr                     { LessThan $1 $3 }
  | Expr '>' Expr                     { GreaterThan $1 $3 }
  | Expr '<=' Expr                    { LTEquals $1 $3 }
  | Expr '>=' Expr                    { GTEquals $1 $3 }
  | BoolExpr '&&' BoolExpr            { And $1 $3 }
  | BoolExpr '||' BoolExpr            { Or $1 $3 }
  | '-' '[' BoolExpr ']' '->' var     { EndRelationQuery $3 $6 }
  | var '-' '[' BoolExpr ']' '->'     { StartRelationQuery $1 $4 }
  | '(' BoolExpr ')'                  { $2 }
  | var '-' '[' BoolExpr ']' '->' var { RelationQuery $1 $4 $7}

NewNode
  : var                               { NodeCopy $1 }
  | NodeAssignments                   { NewNode $1 }

NodeAssignments
  : NodeAssignment                      { [$1] }
  | NodeAssignment ',' NodeAssignments  { ($1 : $3) }

NodeAssignment
  : var '=' Expr                              { NodeAssignment $1 $3 }
  | bigField '=' Expr                         { NodeAssignment $1 $3 }
  | var '-' '[' NodeAssignments ']' '->' var  { RelationAssignment $1 $4 $7 }

IfStatement
  : IF '(' BoolExpr ')' '{' Program '}'                         { IfBlock $3 $6 }
  | IF '(' BoolExpr ')' '{' Program '}' ELSE '{' Program '}'    { IfElseBlock $3 $6 $10 }

ForStatement
  : FOR '(' Type var ':' var ')' '{' Program '}'         { ForBlock $3 $4 $6 $9 }

Type
  : GraphType         { Type $1 }
  | IntegerType       { Type $1 }
  | StringType        { Type $1 }
  | BooleanType       { Type $1 }
  | NodeType          { Type $1 }
  | RelationType      { Type $1 }

{
parseError :: [Token] -> a
parseError [] = error "Should not be erroring on no Tokens"
parseError (Tn t (AlexPn _ r c) : _) = error $ "Parse error on token: " ++ show t ++ ", at: " ++ show r ++ ":" ++ show c ++ "\n"

type Program
  = [Statement]

data Statement
  = Expr Expr
  | IfBlock BoolExpr Program
  | IfElseBlock BoolExpr Program Program
  | ForBlock Type String String Program
  deriving(Eq, Show)

data Expr
  = TypedAssign Type String Expr
  | Assign String Expr
  | Declare Type String
  | Var String
  | Int Int
  | String String
  | MatchQuery String BoolExpr
  | AddQuery String Node
  | ReadFile String
  | Print String
  | BoolExpr BoolExpr
  | GetProperty String String
  deriving(Eq, Show)

data BoolExpr
  = Bool Bool
  | Equals Expr Expr
  | NotEquals Expr Expr
  | LessThan Expr Expr
  | GreaterThan Expr Expr
  | LTEquals Expr Expr
  | GTEquals Expr Expr
  | And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  | EndRelationQuery BoolExpr String
  | StartRelationQuery String BoolExpr
  | RelationQuery String BoolExpr String
  deriving(Eq, Show)
  
data Node
  = NodeCopy String
  | NewNode [NodeAssignment]
  deriving(Eq, Show)

data NodeAssignment
  = NodeAssignment String Expr
  | RelationAssignment String [NodeAssignment] String
  deriving(Eq, Show)

data Type
  = Type Token
  deriving(Eq, Show)

}