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
  CASE                  { Key KeyCaseToken        _ }
  STDOUT                { Key KeySTDOUTToken      _ }
  AND                   { Key KeyLogicalAnd       _ }
  OR                    { Key KeyLogicalOr        _ }
  '('                   { Key KeyBracketLeft      _ }
  ')'                   { Key KeyBracketRight     _ }
  GraphType             { Key KeyGraphTypeToken   _ }
  IntegerType           { Key KeyIntegerTypeToken _ }
  StringType            { Key KeyStringTypeToken  _ }
  BooleanType           { Key KeyBooleanTypeToken _ }
  NodeType              { Key KeyNodeTypeToken    _ }
  RelationType          { Key KeyRelationTypeToken _}
  HAS                   { Key KeyHasToken         _ }
  CONDIF                { Key KeyCONDIFToken      _ }
  CONDELIF              { Key KeyCONDELIFToken    _ }
  THROUGH               { Key KeyTHROUGHToken     _ }
  HEADER                { Key (KeyHeaderToken $$) _ }
  NEGATE                { Key KeyNegateToken      _ }
  argument              { Key (KeyArgument $$)    _ }
  '>>'                  { Key KeyGreaterEqual     _ }
  '<<'                  { Key KeyLessEqual        _ }
  '>'                   { Key KeyGreater          _ }
  '<'                   { Key KeyLess             _ }
  '='                   { Key KeyAssign           _ }
  'i=='                 { Key KeyEqual            _ }
  '!=='                 { Key KeyNotEqual         _ }
  PLUS                  { Key KeyPlusToken        _ }
  SUBT                  { Key KeySubtractToken    _ }
  MULT                  { Key KeyMultiplyToken    _ }
  DIV                   { Key KeyDivideToken      _ }
  '.'                   { Key KeyDot              _ }
  chars                 { Key (KeyChar $$)       _ }
  True                  { Key KeyTrue             _ }
  False                 { Key KeyFalse            _ }
  '-'                   { Key KeyHyphen           _ }
  CALLASSOCIATION       { Key KeyCallAssocToken   _ }
  CALLDATAPOINT         { Key KeyCallDataToken    _ }
  '^'                   { Key KeyEdge             _ }
  '{'                   { Key KeyBraceLeft        _ }
  '}'                   { Key KeyBraceRight       _ }
  ':'                   { Key KeyColon            _ }
  '+='                  { Key KeyIncToken         _ }
  '-='                  { Key KeyDecToken         _ }
  RegularExpression     { Key (KeyRegularToken $$)     _ }

%right '=' '+=' '-='
%left '-'
%left ':'
%right OR
%right AND
%nonassoc 'i==' '!=='
%nonassoc '>' '<' '>>' '<<'
%left PLUS SUBT
%left MULT DIV
%left '.'
%%

Start : GraphType argument '=' ACCESS '(' chars ')' Program { GQLType $2 $6 $8 }

Program : Statement Program  { $1 : $2 }
        | {- empty -}        { []      }

Statement : Expr                    { Expr $1        }
          | IfStatement                  { $1  }
          | ForStatement                 { $1 }
          | STDOUT '(' argument ')' { Print $3      }
        
Expr : FuncAppExpr                          { $1 }
     | BoolExpr       %shift                 { BoolExpr $1 }
     | AssignExpr                            { AssignExpr $1 }
     | Assignable                            { Assignable $1 }
     | LiteralExpr                           { $1 }
     | '(' Expr ')'                          { $2 }

AssignExpr : Type argument '=' Expr                     { TypedAssign $1 $2 $4 }
           | Assignable '=' Expr                        { Assign $1 $3 }
           | Type argument               %shift         { Declare $1 $2 }
           | Assignable '+=' Expr                       { IncrementAssign $1 $3 }
           | Assignable '-=' Expr                       { DecrementAssign $1 $3 }

Assignable
  : argument                 %shift             { Var $1 }
  | argument '.' argument                          { GetProperty $1 $3 }
  | argument '.' HEADER                         { GetProperty $1 $3 }

FuncAppExpr
  : argument '.' CASE '(' BoolExpr ')'        { MatchQuery $1 $5 }
  | argument '.' PLUS '(' Expr ')'              { AddQuery $1 $5 }
  | argument '.' CALLASSOCIATION '(' BoolExpr ')'  { GetRelation $1 $5 }
  | argument '.' NEGATE '(' Expr ')'          { Exclude $1 $5 }

LiteralExpr
  : MathExpr                                   { MathExpr $1         }
  | HEADER                                     { Assignable (Var $1) }
  | chars                                      { String $1           }
  | RegularExpression                          { Regex $1            }
  | argument '.' CALLDATAPOINT '(' Expr ')'    { GetNode $1 $5       }

MathExpr
  : MathTerm                    { $1 }
  | MathExpr PLUS MathExpr       { Addition $1 $3 }
  | MathExpr SUBT MathExpr       { Subtraction $1 $3 }

MathTerm
  : MathExpr MULT MathExpr       { Multiplication $1 $3 }
  | MathExpr DIV MathExpr       { Division $1 $3 }
  | n                         { Int $1 }

BoolExpr
  : Expr AND Expr            { And $1 $3 }
  | Expr OR Expr            { Or $1 $3 }
  | SimpleBoolExpr     %shift               { $1 }

SimpleBoolExpr
  : True                                   { Bool True }
  | False                                  { Bool False }
  | Expr 'i==' Expr                         { Equals $1 $3 }
  | Expr '!==' Expr                         { NotEquals $1 $3}
  | Expr '<' Expr                          { LessThan $1 $3 }
  | Expr '>' Expr                          { GreaterThan $1 $3 }
  | Expr '<<' Expr                         { LTEquals $1 $3 }
  | Expr '>>' Expr                         { GTEquals $1 $3 }
  | '{' BoolExpr '}' '^' argument          { EndRelationQuery $2 $5 }
  | argument '{' BoolExpr '}' '^'          { StartRelationQuery $1 $3 }
  | '(' BoolExpr ')'                       { $2 }
  | argument '{' BoolExpr '}' '^' argument      { RelationQuery $1 $3 $6}
  | Expr '.' HAS '(' StringList ')'   { Contains $1 $5 }

IfStatement
  : CONDIF '(' BoolExpr ')' '{' Program '}'                         { IfBlock $3 $6 }
  | CONDIF '(' BoolExpr ')' '{' Program '}' CONDELIF '{' Program '}'    { IfElseBlock $3 $6 $10 }

ForStatement
  : THROUGH '(' Type argument ':' Expr ')' '{' Program '}'         { ForBlock $3 $4 $6 $9 }

StringList
  : chars                   { [$1] }
  | chars '-' StringList    { ($1 : $3) }

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
parseError (t:_) = error $ "You dizzy, parse error,  at Ln " ++ show (getLn (getPos t)) ++ " Col " ++ show (getCol (getPos t)) ++ " token: " ++ show t
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