{
module Parsing.Grammar where
import Lexing.Tokens
import Parsing.N4jParser
}

%name parser
%tokentype { Token }

%error { parseError }

%token
  int                         { Tn (TokenInt $$)         _ }
  READFILE                    { Tn (TokenReadFile)       _ }
  MATCH                       { Tn (TokenMatch)          _ }
  PRINT                       { Tn (TokenPrint)          _ }
  '&&'                        { Tn (TokenAnd)            _ }
  '||'                        { Tn (TokenOr)             _ }
  '('                         { Tn (TokenLParen)         _ }
  ')'                         { Tn (TokenRParen)         _ }
  GraphType                   { Tn (TokenGraphType)      _ }
  IntegerType                 { Tn (TokenIntegerType)    _ }
  StringType                  { Tn (TokenStringType)     _ }
  BooleanType                 { Tn (TokenBooleanType)    _ }
  '{'                         { Tn (TokenLCurl)          _ }
  '}'                         { Tn (TokenRCurl)          _ }
  IF                          { Tn (TokenIf)             _ }
  ELSE                        { Tn (TokenElse)           _ }
  FOR                         { Tn (TokenFor)            _ }
  ':'                         { Tn (TokenColon)          _ }
  var                         { Tn (TokenVar $$)         _ }
  '>='                        { Tn (TokenGEQ)            _ }
  '<='                        { Tn (TokenLEQ)            _ }
  '>'                         { Tn (TokenGT)             _ }
  '<'                         { Tn (TokenLT)             _ }
  '='                         { Tn (TokenAssign)         _ }
  '=='                        { Tn (TokenEquals)         _ }
  '['                         { Tn (TokenLSQ)            _ }
  ']'                         { Tn (TokenRSQ)            _ }
  '->'                        { Tn (TokenRArrow)         _ }
  '-'                         { Tn (TokenDash)           _ }
  regex                       { Tn (TokenRegex $$)       _ }
  ADD                         { Tn (TokenAdd)            _ }
  '.'                         { Tn (TokenDot)            _ }
  bigField                    { Tn (TokenBigField $$)    _ }
  string                      { Tn (TokenString $$)      _ }
  True                        { Tn (TokenTrue)           _ }
  False                       { Tn (TokenFalse)          _ }
  ';'                         { Tn (TokenSemicolon)      _ }
  '!='                        { Tn (TokenNotEquals)      _ }
  ','                         { Tn (TokenComma)          _ }
  NodeType                    { Tn (TokenNodeType)       _ }
  RelationType                { Tn (TokenRelationType)   _ }
  GETRELATION                 { Tn (TokenGetRelation)    _ }
  CONTAINS                    { Tn (TokenContains)       _ }
  GETNODE                     { Tn (TokenGetNode)        _ }
  '+'                         { Tn (TokenPlus)           _ }
  '*'                         { Tn (TokenStar)           _ }
  '/'                         { Tn (TokenFSlash)         _ }
  '+='                        { Tn (TokenIncrement)      _ }
  '-='                        { Tn (TokenDecrement)      _ }
  EXCLUDE                     { Tn (TokenExclude)        _ }
  NEW                         { Tn (TokenNew)            _ }
  intField                    { Tn (TokenIntField $$)    _ }
  boolField                   { Tn (TokenBoolField $$)   _ }
  stringField                 { Tn (TokenStringField $$) _ }

%right '=' '+=' '-='
%left ','
%left ':'
%right '||'
%right '&&'
%nonassoc '==' '!='
%nonassoc '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/'
%left '.'
%%

GQL
  : GraphType var '=' READFILE '(' string ')' ';' Program          { GQLType $2 $6 $9 }

Program
  : Statement Program               { ($1 : $2) }
  | {- empty -}                     { [] }

Statement
  : Expr ';'                        { Expr $1 }
  | IfStatement                     { $1 }
  | ForStatement                    { $1 }
  | PRINT '(' var ')' ';'           { Print $3 }


Expr
  : FuncAppExpr                           { $1 }
  | BoolExpr       %shift                 { BoolExpr $1 }
  | AssignExpr                            { AssignExpr $1 }
  | Assignable                            { Assignable $1 }
  | LiteralExpr                           { $1 }
  | '(' Expr ')'                          { $2 }

LiteralExpr
  : MathExpr                              { MathExpr $1 }
  | bigField                              { Assignable (Var $1) }
  | string                                { String $1 }
  | regex                                 { Regex $1 }
  | var '.' GETNODE '(' Expr ')'          { GetNode $1 $5 }

-- data LabelType
--   = StringLabel
--   | IntLabel
--   | BoolLabel
--   | NullLabel -- for big fields
--   deriving(Eq, Show)

Header
  : bigField ',' Header                   { ($1, NullLabel) : $3 }
  | intField ',' Header                   { ($1, IntLabel) : $3 }
  | stringField ',' Header                { ($1, StringLabel) : $3 }
  | boolField ',' Header                  { ($1, BoolLabel) : $3 }
  | bigField                              { [($1, NullLabel)] }
  | intField                              { [($1, IntLabel)] }
  | stringField                           { [($1, StringLabel)] }
  | boolField                             { [($1, BoolLabel)] }

AssignExpr
  : Type var '=' Expr                 %shift                { TypedAssign $1 $2 $4 }
  | Assignable '=' Expr                                     { Assign $1 $3 }
  | Type var               %shift                           { Declare $1 $2 }
  | Assignable '+=' Expr                                    { IncrementAssign $1 $3 }
  | Assignable '-=' Expr                                    { DecrementAssign $1 $3 }
  | RelationType var '=' NEW RelationType '(' Header ')'    { RelationDeclare $2 $7 }
  | NodeType var '=' NEW NodeType '(' Header ')'            { NodeDeclare $2 $7 }

Assignable
  : var                 %shift           { Var $1 }
  | var '.' var                          { GetProperty $1 $3 }
  | var '.' bigField                     { GetProperty $1 $3 }

FuncAppExpr
  : var '.' MATCH '(' BoolExpr ')'        { MatchQuery $1 $5 }
  | var '.' ADD '(' Expr ')'              { AddQuery $1 $5 }
  | var '.' GETRELATION '(' BoolExpr ')'  { GetRelation $1 $5 }
  | var '.' EXCLUDE '(' Expr ')'          { Exclude $1 $5 }

MathExpr
  : MathTerm                    { $1 }
  | MathExpr '+' MathExpr       { Addition $1 $3 }
  | MathExpr '-' MathExpr       { Subtraction $1 $3 }

MathTerm
  : MathExpr '*' MathExpr       { Multiplication $1 $3 }
  | MathExpr '/' MathExpr       { Division $1 $3 }
  | int                         { Int $1 }

BoolExpr
  : Expr '&&' Expr            { And $1 $3 }
  | Expr '||' Expr            { Or $1 $3 }
  | SimpleBoolExpr     %shift               { $1 }

SimpleBoolExpr
  : True                                      { Bool True }
  | False                                     { Bool False }
  | Expr '==' Expr                            { Equals $1 $3 }
  | Expr '!=' Expr                            { NotEquals $1 $3}
  | Expr '<' Expr                             { LessThan $1 $3 }
  | Expr '>' Expr                             { GreaterThan $1 $3 }
  | Expr '<=' Expr                            { LTEquals $1 $3 }
  | Expr '>=' Expr                            { GTEquals $1 $3 }
  | '-' '[' BoolExpr ']' '->' var ':' var     { EndRelationQuery $3 $6 $8 }
  | var '-' '[' BoolExpr ']' '->' ':' var     { StartRelationQuery $1 $4 $8 }
  | '(' BoolExpr ')'                          { $2 }
  | var '-' '[' BoolExpr ']' '->' var ':' var { RelationQuery $1 $4 $7 $9 }
  | Expr '.' CONTAINS '(' StringList ')'      { Contains $1 $5 }

IfStatement
  : IF '(' BoolExpr ')' '{' Program '}'                         { IfBlock $3 $6 }
  | IF '(' BoolExpr ')' '{' Program '}' ELSE '{' Program '}'    { IfElseBlock $3 $6 $10 }

ForStatement
  : FOR '(' Type var ':' Expr ')' '{' Program '}'         { ForBlock $3 $4 $6 $9 }

StringList
  : string                   { [$1] }
  | string ',' StringList    { ($1 : $3) }

Type
  : GraphType           { Type $1 }
  | IntegerType         { Type $1 }
  | StringType          { Type $1 }
  | BooleanType         { Type $1 }
  | NodeType     %shift { Type $1 }
  | RelationType %shift { Type $1 }

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
  | ForBlock Type String Expr Program
  | Print String
  deriving(Eq, Show)

data Expr
  = MathExpr MathExpr
  | String String
  | Regex String
  | MatchQuery String BoolExpr
  | AddQuery String Expr
  | BoolExpr BoolExpr
  | GetRelation String BoolExpr
  | Exclude String Expr
  | GetNode String Expr
  | AssignExpr AssignExpr
  | Assignable Assignable
  deriving(Eq, Show)

data AssignExpr
  = TypedAssign Type String Expr
  | IncrementAssign Assignable Expr
  | DecrementAssign Assignable Expr
  | Assign Assignable Expr
  | Declare Type String
  | RelationDeclare String Headers
  | NodeDeclare String Headers
  deriving(Eq, Show)

data Assignable
  = Var String
  | GetProperty String String
  deriving(Eq, Show)

data MathExpr
  = Int Int
  | Addition MathExpr MathExpr
  | Subtraction MathExpr MathExpr
  | Multiplication MathExpr MathExpr
  | Division MathExpr MathExpr
  deriving(Eq, Show)

data BoolExpr
  = Bool Bool
  | Equals Expr Expr
  | NotEquals Expr Expr
  | LessThan Expr Expr
  | GreaterThan Expr Expr
  | LTEquals Expr Expr
  | GTEquals Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | EndRelationQuery BoolExpr String String
  | StartRelationQuery String BoolExpr String
  | RelationQuery String BoolExpr String String
  | Contains Expr [String]
  deriving(Eq, Show)

data Type
  = Type Token
  deriving(Eq, Show)

data GQL
  = GQLType String String Program
  deriving(Eq, Show)

}