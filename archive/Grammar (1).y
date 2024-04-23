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
  GETRELATION                 { Tn (TokenGetRelation)  _ }
  CONTAINS                    { Tn (TokenContains)     _ }
  GETNODE                     { Tn (TokenGetNode)      _ }
  '+'                         { Tn (TokenPlus)         _ }
  '*'                         { Tn (TokenStar)         _ }
  '/'                         { Tn (TokenFSlash)       _ }
  '+='                        { Tn (TokenIncrement)    _ }
  '-='                        { Tn (TokenDecrement)    _ }
  EXCLUDE                     { Tn (TokenExclude)      _ }

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

Program
  : Statement Program               { ($1 : $2) }
  | {- empty -}                     { [] }

Statement
  : Expr ';'                        { Expr $1 }
  | IfStatement                     { $1 }
  | ForStatement                    { $1 }


Expr
  : ExtractExpr                           { $1 }
  | FuncExpr                              { $1 }
  | FuncAppExpr                           { $1 }
  | BoolExpr       %shift                 { BoolExpr $1 }
  | AssignExpr                            { $1 }
  | LiteralExpr                           { $1 }
  | '(' Expr ')'                          { $2 }

LiteralExpr
  : var                                   { Var $1 }
  | MathExpr                              { MathExpr $1 }
  | bigField                              { Var $1 }
  | string                                { String $1 }
  | regex                                 { Regex $1 }

AssignExpr
  : Type var '=' Expr                     { TypedAssign $1 $2 $4 }
  | Expr '=' Expr                         { Assign $1 $3 }
  | Type var               %shift         { Declare $1 $2 }
  | Expr '+=' Expr                        { IncrementAssign $1 $3 }
  | Expr '-=' Expr                        { DecrementAssign $1 $3 }
  | Expr '.' GETNODE '(' Expr ')'         { GetNode $1 $5 }

ExtractExpr
  : Expr '.' var                          { GetProperty $1 $3 }
  | Expr '.' bigField                     { GetProperty $1 $3 }

FuncExpr
  : READFILE '(' string ')'               { ReadFile $3 }
  | PRINT '(' var ')'                     { Print $3 }

FuncAppExpr
  : Expr '.' MATCH '(' BoolExpr ')'        { MatchQuery $1 $5 }
  | Expr '.' ADD '(' Expr ')'              { AddQuery $1 $5 }
  | Expr '.' GETRELATION '(' BoolExpr ')'  { GetRelation $1 $5 }
  | Expr '.' EXCLUDE '(' Expr ')'          { Exclude $1 $5 }

MathExpr
  : MathTerm                    { $1 }
  | MathExpr '+' MathExpr       { Addition $1 $3 }
  | MathExpr '-' MathExpr       { Subtraction $1 $3 }

MathTerm
  : MathExpr '*' MathExpr       { Multiplication $1 $3 }
  | MathExpr '/' MathExpr       { Divison $1 $3 }
  | int                         { Int $1 }

BoolExpr
  : SimpleBoolExpr '&&' BoolExpr            { And $1 $3 }
  | SimpleBoolExpr '||' BoolExpr            { Or $1 $3 }
  | SimpleBoolExpr     %shift               { $1 }

SimpleBoolExpr
  : True                                   { Bool True }
  | False                                  { Bool False }
  | Expr '==' Expr                         { Equals $1 $3 }
  | Expr '!=' Expr                         { NotEquals $1 $3}
  | Expr '<' Expr                          { LessThan $1 $3 }
  | Expr '>' Expr                          { GreaterThan $1 $3 }
  | Expr '<=' Expr                         { LTEquals $1 $3 }
  | Expr '>=' Expr                         { GTEquals $1 $3 }
  | '-' '[' BoolExpr ']' '->' var          { EndRelationQuery $3 $6 }
  | var '-' '[' BoolExpr ']' '->'          { StartRelationQuery $1 $4 }
  | '(' BoolExpr ')'                       { $2 }
  | var '-' '[' BoolExpr ']' '->' var      { RelationQuery $1 $4 $7}
  | Expr '.' CONTAINS '(' StringList ')'   { Contains $1 $5 }

IfStatement
  : IF '(' BoolExpr ')' '{' Program '}'                         { IfBlock $3 $6 }
  | IF '(' BoolExpr ')' '{' Program '}' ELSE '{' Program '}'    { IfElseBlock $3 $6 $10 }

ForStatement
  : FOR '(' Type var ':' Expr ')' '{' Program '}'         { ForBlock $3 $4 $6 $9 }

StringList
  : string                   { [$1] }
  | string ',' StringList    { ($1 : $3) }

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
  | ForBlock Type String Expr Program
  deriving(Eq, Show)

data Expr
  = TypedAssign Type String Expr
  | Assign Expr Expr
  | Declare Type String
  | Var String
  | MathExpr MathExpr
  | String String
  | Regex String
  | MatchQuery Expr BoolExpr
  | AddQuery Expr Expr
  | ReadFile String
  | Print String
  | BoolExpr BoolExpr
  | GetProperty Expr String
  | GetRelation Expr BoolExpr
  | IncrementAssign Expr Expr
  | DecrementAssign Expr Expr
  | Exclude Expr Expr
  | GetNode Expr Expr
  deriving(Eq, Show)

data MathExpr
  = Int Int
  | Addition MathExpr MathExpr
  | Subtraction MathExpr MathExpr
  | Multiplication MathExpr MathExpr
  | Divison MathExpr MathExpr
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
  | Contains Expr [String]
  deriving(Eq, Show)

data Type
  = Type Token
  deriving(Eq, Show)

}