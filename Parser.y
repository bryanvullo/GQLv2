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
    : Expression               { Expression $1  }  -- Expression statement
    | IfStatement              { $1             }  -- If statement
    | ForStatement             { $1             }  -- For loop statement  
    | STDOUT '(' argument ')'  { Print $3       }  -- Print statement
        
Expression
    : Application               { $1                    }  -- Function application expression
    | BooleanExpression         { BooleanExpression $1  }  -- Boolean expression
    | SetterExpression          { SetterExpression $1   }  -- Assignment expression
    | SettableExpression        { SettableExpression $1 }  -- SettableExpression expression
    | LitExpression             { $1                    }  -- Literal expression
    | '(' Expression ')'        { $2                    }  -- Parenthesized expression

SetterExpression
    : Type argument '=' Expression            { TypedSet $1 $2 $4     }  -- Typed assignment
    | SettableExpression '=' Expression       { Set $1 $3             }  -- Regular assignment
    | Type argument                           { Declare $1 $2         }  -- Variable declaration
    | SettableExpression '++' Expression      { IncrSet $1 $3         }  -- Increment assignment
    | SettableExpression '--' Expression      { DecrSet $1 $3         }  -- Decrement assignment

SettableExpression
    : argument                               { Var $1                }  -- Variable
    | argument '.' argument                  { GetProperty $1 $3     }  -- Property access
    | argument '.' HEADER                    { GetProperty $1 $3     }  -- Property access with header

Application
    : argument '.' CASE '(' BooleanExpression ')'                 { CaseQuery $1 $5       }  -- Match query
    | argument '.' PLUS '(' Expression ')'                        { AddQuery $1 $5        }  -- Add query
    | argument '.' CALLASSOCIATION '(' BooleanExpression ')'      { GetRelation $1 $5     }  -- Get relation  
    | argument '.' NEGATE '(' Expression ')'                      { Exclude $1 $5         }  -- Exclude expression

LitExpression
    : NumExpression                                    { NumExpression $1            }  -- Mathematical expression
    | HEADER                                           { SettableExpression (Var $1) }  -- Header literal
    | chars                                            { String $1                   }  -- String literal
    | RegularExpression                                { Regex $1                    }  -- Regular expression literal
    | argument '.' CALLDATAPOINT '(' Expression ')'    { GetNode $1 $5               }  -- Get node expression

NumExpression
    : MathTerm                              { $1                }  -- Mathematical term
    | NumExpression PLUS NumExpression      { Addition $1 $3    }  -- Addition
    | NumExpression SUBT NumExpression      { Subtraction $1 $3 }  -- Subtraction

MathTerm 
    : NumExpression MULT NumExpression      { Multiplication $1 $3 }  -- Multiplication
    | NumExpression DIV NumExpression       { Division $1 $3       }  -- Division
    | n                                     { Int $1               }  -- Integer literal

BooleanExpression
    : Expression AND Expression               { And $1 $3        }  -- Logical AND  
    | Expression OR Expression                { Or $1 $3         }  -- Logical OR
    | SimpleBoolExpr                          { $1               }  -- Simple boolean expression

SimpleBoolExpr
    : True                                             { Bool True               }  -- Boolean true literal
    | False                                            { Bool False              }  -- Boolean false literal
    | Expression 'i==' Expression                      { Equals $1 $3            }  -- Equality comparison
    | Expression '!==' Expression                      { NotEquals $1 $3         }  -- Inequality comparison  
    | Expression '<' Expression                        { LessThan $1 $3          }  -- Less than comparison
    | Expression '>' Expression                        { GreaterThan $1 $3       }  -- Greater than comparison
    | Expression '<<' Expression                       { LTEquals $1 $3          }  -- Less than or equal to comparison
    | Expression '>>' Expression                       { GTEquals $1 $3          }  -- Greater than or equal to comparison
    | '{' BooleanExpression '}' '^' argument           { EndRelationQuery $2 $5  }  -- End relation query
    | argument '{' BooleanExpression '}' '^'           { StartRelationQuery $1 $3}  -- Start relation query
    | '(' BooleanExpression ')'                        { $2                      }  -- Parenthesized boolean expression
    | argument '{' BooleanExpression '}' '^' argument  { RelationQuery $1 $3 $6  }  -- Relation query
    | Expression '.' HAS '(' StringList ')'            { Contains $1 $5          }  -- Contains expression

IfStatement
    : CONDIF '(' BooleanExpression ')' '{' Program '}'                          { IfBlock $3 $6         }  -- If block
    | CONDIF '(' BooleanExpression ')' '{' Program '}' CONDELIF '{' Program '}' { IfElseBlock $3 $6 $10 }  -- If-else block

ForStatement  
    : THROUGH '(' Type argument ':' Expression ')' '{' Program '}'              { ForBlock $3 $4 $6 $9  }  -- For loop block

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
parseError [] = error "parse error at end of input"
parseError (t:_) = error $ "parse error  at Ln " ++ show (getLn (getPos t)) ++ " Col " ++ show (getCol (getPos t)) ++ " token: " ++ show t
  where
    getPos (Key _ p) = p
    getLn (AlexPn _ l _) = l  
    getCol (AlexPn _ _ c) = c

-- Abstract syntax tree data types
type Program 
  = [Statement]

data Statement
  = Expression Expression
  | IfBlock BooleanExpression Program
  | IfElseBlock BooleanExpression Program Program
  | ForBlock Type String Expression Program  
  | Print String
  deriving(Eq, Show)

data Expression
  = NumExpression NumExpression
  | String String
  | Regex String
  | CaseQuery String BooleanExpression
  | AddQuery String Expression
  | BooleanExpression BooleanExpression
  | GetRelation String BooleanExpression
  | Exclude String Expression
  | GetNode String Expression
  | SetterExpression SetterExpression
  | SettableExpression SettableExpression  
  deriving(Eq, Show)

data SetterExpression
  = TypedSet Type String Expression
  | IncrSet SettableExpression Expression
  | DecrSet SettableExpression Expression
  | Set SettableExpression Expression
  | Declare Type String
  deriving(Eq, Show)

data SettableExpression
  = Var String
  | GetProperty String String
  deriving(Eq, Show)

data NumExpression
  = Int Int
  | Addition NumExpression NumExpression
  | Subtraction NumExpression NumExpression
  | Multiplication NumExpression NumExpression
  | Division NumExpression NumExpression
  deriving(Eq, Show)

data BooleanExpression  
  = Bool Bool
  | Equals Expression Expression
  | NotEquals Expression Expression
  | LessThan Expression Expression
  | GreaterThan Expression Expression 
  | LTEquals Expression Expression
  | GTEquals Expression Expression
  | And Expression Expression
  | Or Expression Expression
  | EndRelationQuery BooleanExpression String
  | StartRelationQuery String BooleanExpression
  | RelationQuery String BooleanExpression String
  | Contains Expression [String]  
  deriving(Eq, Show)

data Type
  = Type Token
  deriving(Eq, Show)

data Start
  = StartExpr String String Program  
  deriving(Eq, Show)

}