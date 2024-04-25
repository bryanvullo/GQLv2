{
module Parser where
import Lexer
}

%name parser
%tokentype { Token      }
%error     { parseError }

-- Token declarations
%token
  n                     { Key (XNum $$)               _ }  -- Number literal
  chars                 { Key (XChar $$)              _ }  -- Character literal
  RegularExpression     { Key (XRegularExpression $$) _ }  -- Regular expression literal
  argument              { Key (XArgument $$)          _ }  -- Argument
  HEADER                { Key (XHeader $$)            _ }  -- Header
  True                  { Key XBooleanTrue            _ }  -- "True" boolean literal
  False                 { Key XBooleanFalse           _ }  -- "False" boolean literal
  GraphType             { Key XDataGraph              _ }  -- Graph type
  IntegerType           { Key XInt                    _ }  -- Integer type
  StringType            { Key XStr                    _ }  -- String type 
  BooleanType           { Key XBool                   _ }  -- Boolean type
  NodeType              { Key XNode                   _ }  -- Node type
  RelationType          { Key XDataAssociation        _ }  -- Relation type
  ACCESS                { Key XACCESS                 _ }  -- "ACCESS" keyword
  CASE                  { Key XCASE                   _ }  -- "CASE" keyword
  STDOUT                { Key XSTDOUT                 _ }  -- "STDOUT" keyword
  HAS                   { Key XHAS                    _ }  -- "HAS" keyword
  CONDIF                { Key XCONDIF                 _ }  -- "CONDIF" keyword
  CONDELIF              { Key XCONDELIF               _ }  -- "CONDELIF" keyword
  THROUGH               { Key XTHROUGH                _ }  -- "THROUGH" keyword
  NEGATE                { Key XNegate                 _ }  -- "NEGATE" keyword  
  CALLASSOCIATION       { Key XAssociationCheck       _ }  -- "CALLASSOCIATION" keyword
  CALLDATAPOINT         { Key XDataNodeCheck          _ }  -- "CALLDATAPOINT" keyword  
  AND                   { Key XLogicalAND             _ }  -- "AND" logical operator
  OR                    { Key XLogicalOR              _ }  -- "OR" logical operator
  PLUS                  { Key XPlus                   _ }  -- "+" operator
  SUBT                  { Key XSubtract               _ }  -- "-" operator 
  MULT                  { Key XMultiply               _ }  -- "*" operator
  DIV                   { Key XDivide                 _ }  -- "/" operator
  '('                   { Key XBracketLeft            _ }  -- Left parenthesis
  ')'                   { Key XBracketRight           _ }  -- Right parenthesis
  '>>'                  { Key XSlackGreater           _ }  -- ">>" operator
  '<<'                  { Key XSlackLesser            _ }  -- "<<" operator
  '>'                   { Key XStrictGreater          _ }  -- ">" operator
  '<'                   { Key XStrictLesser           _ }  -- "<" operator
  '='                   { Key XSet                    _ }  -- "=" operator
  'i=='                 { Key XLogicalEquation        _ }  -- "i==" equality operator
  '!=='                 { Key XLogicalInequation      _ }  -- "!==" inequality operator
  '.'                   { Key XPeriod                 _ }  -- "." operator
  '-'                   { Key XHyphen                 _ }  -- "-" operator
  '^'                   { Key XAssociationSymbol      _ }  -- "^" operator
  '{'                   { Key XBraceLeft              _ }  -- Left brace
  '}'                   { Key XBraceRight             _ }  -- Right brace
  ':'                   { Key XColonSymbol            _ }  -- ":" symbol
  '++'                  { Key XNumericalIncrement     _ }  -- "++" increment operator
  '--'                  { Key XNumericalDecrement     _ }  -- "--" decrement operator

-- Precedence and associativity rules  
%right '=' '++' '--' '-' ':' 
%right OR AND
%nonassoc 'i==' '!==' '>' '<' '>>' '<<'
%left PLUS SUBT MULT DIV '.'

%%  

-- Grammar rules
Start 
    : GraphType argument '=' ACCESS '(' chars ')' Program  { StartExpr $2 $6 $8 }
      -- Start symbol, defines the structure of a GQL program

Program 
    : Statement Program  { $1 : $2 }  -- Program consists of multiple statements
    | {- empty -}        { []      }  -- Empty program

Statement
    : Expr                     { Expr $1        }  -- Expression statement
    | IfStatement              { $1             }  -- If statement
    | ForStatement             { $1             }  -- For loop statement  
    | STDOUT '(' argument ')'  { Print $3       }  -- Print statement
        
Expr
    : Application               { $1             }  -- Function application expression
    | BoolExpr                  { BoolExpr $1    }  -- Boolean expression
    | AssignExpr                { AssignExpr $1  }  -- Assignment expression
    | Assignable                { Assignable $1  }  -- Assignable expression
    | LiteralExpr               { $1             }  -- Literal expression
    | '(' Expr ')'              { $2             }  -- Parenthesized expression

AssignExpr
    : Type argument '=' Expr                 { TypedAssign $1 $2 $4  }  -- Typed assignment
    | Assignable '=' Expr                    { Assign $1 $3          }  -- Regular assignment
    | Type argument                          { Declare $1 $2         }  -- Variable declaration
    | Assignable '++' Expr                   { IncrementAssign $1 $3 }  -- Increment assignment
    | Assignable '--' Expr                   { DecrementAssign $1 $3 }  -- Decrement assignment

Assignable
    : argument                               { Var $1                }  -- Variable
    | argument '.' argument                  { GetProperty $1 $3     }  -- Property access
    | argument '.' HEADER                    { GetProperty $1 $3     }  -- Property access with header

Application
    : argument '.' CASE '(' BoolExpr ')'           { MatchQuery $1 $5      }  -- Match query
    | argument '.' PLUS '(' Expr ')'               { AddQuery $1 $5        }  -- Add query
    | argument '.' CALLASSOCIATION '(' BoolExpr ')'{ GetRelation $1 $5     }  -- Get relation  
    | argument '.' NEGATE '(' Expr ')'             { Exclude $1 $5         }  -- Exclude expression

LiteralExpr
    : MathExpr                                    { MathExpr $1           }  -- Mathematical expression
    | HEADER                                      { Assignable (Var $1)   }  -- Header literal
    | chars                                       { String $1             }  -- String literal
    | RegularExpression                           { Regex $1              }  -- Regular expression literal
    | argument '.' CALLDATAPOINT '(' Expr ')'     { GetNode $1 $5         }  -- Get node expression

MathExpr
    : MathTerm                    { $1                }  -- Mathematical term
    | MathExpr PLUS MathExpr      { Addition $1 $3    }  -- Addition
    | MathExpr SUBT MathExpr      { Subtraction $1 $3 }  -- Subtraction

MathTerm 
    : MathExpr MULT MathExpr      { Multiplication $1 $3 }  -- Multiplication
    | MathExpr DIV MathExpr       { Division $1 $3       }  -- Division
    | n                           { Int $1               }  -- Integer literal

BoolExpr
    : Expr AND Expr               { And $1 $3        }  -- Logical AND  
    | Expr OR Expr                { Or $1 $3         }  -- Logical OR
    | SimpleBoolExpr              { $1               }  -- Simple boolean expression

SimpleBoolExpr
    : True                                    { Bool True               }  -- Boolean true literal
    | False                                   { Bool False              }  -- Boolean false literal
    | Expr 'i==' Expr                         { Equals $1 $3            }  -- Equality comparison
    | Expr '!==' Expr                         { NotEquals $1 $3         }  -- Inequality comparison  
    | Expr '<' Expr                           { LessThan $1 $3          }  -- Less than comparison
    | Expr '>' Expr                           { GreaterThan $1 $3       }  -- Greater than comparison
    | Expr '<<' Expr                          { LTEquals $1 $3          }  -- Less than or equal to comparison
    | Expr '>>' Expr                          { GTEquals $1 $3          }  -- Greater than or equal to comparison
    | '{' BoolExpr '}' '^' argument           { EndRelationQuery $2 $5  }  -- End relation query
    | argument '{' BoolExpr '}' '^'           { StartRelationQuery $1 $3}  -- Start relation query
    | '(' BoolExpr ')'                        { $2                      }  -- Parenthesized boolean expression
    | argument '{' BoolExpr '}' '^' argument  { RelationQuery $1 $3 $6  }  -- Relation query
    | Expr '.' HAS '(' StringList ')'         { Contains $1 $5          }  -- Contains expression

IfStatement
    : CONDIF '(' BoolExpr ')' '{' Program '}'                          { IfBlock $3 $6         }  -- If block
    | CONDIF '(' BoolExpr ')' '{' Program '}' CONDELIF '{' Program '}' { IfElseBlock $3 $6 $10 }  -- If-else block

ForStatement  
    : THROUGH '(' Type argument ':' Expr ')' '{' Program '}'           { ForBlock $3 $4 $6 $9  }  -- For loop block

StringList
    : chars                    { [$1]     }  -- Single string in the list
    | chars '-' StringList     { ($1 : $3)}  -- Multiple strings in the list

Type
    : GraphType         { Type $1 }  -- Graph type
    | IntegerType       { Type $1 }  -- Integer type  
    | StringType        { Type $1 }  -- String type
    | BooleanType       { Type $1 }  -- Boolean type
    | NodeType          { Type $1 }  -- Node type
    | RelationType      { Type $1 }  -- Relation type

-- Parser monad and utility functions
{

parseError :: [Token] -> a
parseError [] = error "Kmt parse error at end of input you paigan"
parseError (t:_) = error $ "You dizzy fam, u got a parse error  at Ln " ++ show (getLn (getPos t)) ++ " Col " ++ show (getCol (getPos t)) ++ " token: " ++ show t ++ " Kmt parse error you paigan"
  where
    getPos (Key _ p) = p
    getLn (AlexPn _ l _) = l  
    getCol (AlexPn _ _ c) = c

-- Abstract syntax tree data types
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

data Start
  = StartExpr String String Program  
  deriving(Eq, Show)

}