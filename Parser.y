{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token
  n                     { Key (KeyNum $$)         _ }
  ACCESS                { Key KeyACCESSToken      _ }
  CASE                  { Key KeyCASEToken        _ }
  STDOUT                { Key KeySTDOUTToken      _ }
  AND                   { Key KeyLogicalAnd       _ }
  OR                    { Key KeyLogicalOr        _ }
  '('                   { Key KeyBracketLeft      _ }
  ')'                   { Key KeyBracketRight     _ }
  Graph                 { Key KeyGraphToken       _ }
  HAS                   { Key KeyHasToken         _ }
  CONDIF                { Key KeyCONDIFToken      _ }
  THROUGH               { Key KeyTHROUGHToken     _ }
  identity              { Key (KeyIdentity $$)    _ }
  '>>'                  { Key KeyGreaterEqual     _ }
  '<<'                  { Key KeyLessEqual        _ }
  '>'                   { Key KeyGreater          _ }
  '<'                   { Key KeyLess             _ }
  '='                   { Key KeyAssign           _ }
  'i=='                 { Key KeyEqual            _ }
  '!=='                 { Key KeyNotEqual         _ }
  PLUS                  { Key KeyPlusToken        _ }
  '.'                   { Key KeyDot              _ }
  chars                 { Key (KeyChars $$)       _ }
  True                  { Key KeyTrue             _ }
  False                 { Key KeyFalse            _ }
  '-'                   { Key KeyMinus            _ }
  DataPoint             { Key KeyDataPointToken   _ }
  Association           { Key KeyAssociationToken _ }
  CALLASSOCIATION       { Key KeyCallAssocToken   _ }
  CALLDATAPOINT         { Key KeyCallDataToken    _ }
  '^'                   { Key KeyEdge             _ }
  '{'                   { Key KeyBraceLeft        _ }
  '}'                   { Key KeyBraceRight       _ }
  ':'                   { Key KeyColon            _ }

%right '='
%left OR AND
%nonassoc '>' '<' '>>' '<<' 'i==' '!==' 
%left '+' '-'

%%

Start : Graph identity '=' ACCESS '(' chars ')' Program { StartProgram XXX}

Program : Statement Program  { $1 : $2 }
        | {- empty -}        { []     }

Statement : Expr                    { Expr $1        }
          | Condif                  { CondifStmt $1  }
          | Through                 { ThroughStmt $1 }
          | STDOUT '(' identity ')' { Stdout $3      }
        
Expr : FuncAppExpr                           { $1 }
    | BoolExpr       %shift                 { BoolExpr $1 }
    | AssignExpr                            { AssignExpr $1 }
    | Assignable                            { Assignable $1 }
    | LiteralExpr                           { $1 }
    | '(' Expr ')'                          { $2 }

AssignExpr
  : Type var '=' Expr                     { TypedAssign $1 $2 $4 }
  | Assignable '=' Expr                   { Assign $1 $3 }
  | Type var               %shift         { Declare $1 $2 }
  | Assignable '+=' Expr                  { IncrementAssign $1 $3 }
  | Assignable '-=' Expr                  { DecrementAssign $1 $3 }

Assignable
  : var                 %shift           { Var $1 }
  | var '.' var                          { GetProperty $1 $3 }
  | var '.' bigField                     { GetProperty $1 $3 }

FuncAppExpr
  : var '.' MATCH '(' BoolExpr ')'        { MatchQuery $1 $5 }
  | var '.' ADD '(' Expr ')'              { AddQuery $1 $5 }
  | var '.' GETRELATION '(' BoolExpr ')'  { GetRelation $1 $5 }
  | var '.' EXCLUDE '(' Expr ')'          { Exclude $1 $5 }

LiteralExpr
  : MathExpr                              { MathExpr $1 }
  | bigField                              { Assignable (Var $1) }
  | string                                { String $1 }
  | regex                                 { Regex $1 }
  | var '.' GETNODE '(' Expr ')'         { GetNode $1 $5 }

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
parseError [] = error "Kmt parse error at end of input you paigan"
parseError (t:_) = error $ "You dizzy parse error at Ln " ++ show (getLn (getPos t)) ++ " Col " ++ show (getCol (getPos t)) ++ " token: " ++ show t
  where
    getPos (Key _ p) = p
    getLn (AlexPn _ l _) = l
    getCol (AlexPn _ _ c) = c

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
  | EndRelationQuery BoolExpr String
  | StartRelationQuery String BoolExpr
  | RelationQuery String BoolExpr String
  | Contains Expr [String]
  deriving(Eq, Show)

data Type
  = Type Token
  deriving(Eq, Show)

data GQL
  = GQLType String String Program
  deriving(Eq, Show)

}