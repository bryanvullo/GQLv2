{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }

%error { parseError }

%token
  int                         { Tok _ (TokInt $$)       _ }
  READFILE                    { Tok _ TokReadFile       _ }
  MATCH                       { Tok _ TokMatch          _ }
  PRINT                       { Tok _ TokPrint          _ }
  '"'                         { Tok _ TokDelimiter      _ }
  '&&'                        { Tok _ TokAnd            _ }
  '||'                        { Tok _ TokOr             _ }
  '('                         { Tok _ TokLParen         _ }
  ')'                         { Tok _ TokRParen         _ }
  GraphType                   { Tok _ TokGraphType      _ }
  IntegerType                 { Tok _ TokIntegerType    _ }
  StringType                  { Tok _ TokStringType     _ }
  BooleanType                 { Tok _ TokBooleanType    _ }
  '{'                         { Tok _ TokLCurl          _ }
  '}'                         { Tok _ TokRCurl          _ }
  IF                          { Tok _ TokIf             _ }
  ELSE                        { Tok _ TokElse           _ }
  FOR                         { Tok _ TokFor            _ }
  ':'                         { Tok _ TokColon          _ }
  var                         { Tok _ (TokIdent $$)     _ }
  '>='                        { Tok _ TokGEQ            _ }
  '<='                        { Tok _ TokLEQ            _ }
  '>'                         { Tok _ TokGT             _ }
  '<'                         { Tok _ TokLT             _ }
  '='                         { Tok _ TokAssign         _ }
  '=='                        { Tok _ TokEquals         _ }
  '['                         { Tok _ TokLSQ            _ }
  ']'                         { Tok _ TokRSQ            _ }
  '->'                        { Tok _ TokRArrow         _ }
  '-'                         { Tok _ TokDash           _ }
  regex                       { Tok _ (TokRegex $$)     _ }
  ADD                         { Tok _ TokAdd            _ }
  '.'                         { Tok _ TokDot            _ }
  bigField                    { Tok _ (TokBigField $$)  _ }
  string                      { Tok _ (TokString $$)    _ }
  True                        { Tok _ TokTrue           _ }
  False                       { Tok _ TokFalse          _ }
  ';'                         { Tok _ TokSemicolon      _ }
  '!='                        { Tok _ TokNotEquals      _ }
  ','                         { Tok _ TokComma          _ }
  NodeType                    { Tok _ TokNodeType       _ }
  RelationType                { Tok _ TokRelationType   _ }

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
parseError (Tok (AlexPn _ r c) t : _) = error $ "Parse error on token: " ++ show t ++ ", at: " ++ show r ++ ":" ++ show c ++ "\n"

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