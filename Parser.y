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
  GraphClass            { Key XDataGraph              _ }  -- Graph class
  IntegerClass          { Key XInt                    _ }  -- Integer class
  StringClass           { Key XStr                    _ }  -- String class 
  BooleanClass          { Key XBool                   _ }  -- Boolean class
  NodeClass             { Key XNode                   _ }  -- Node class
  RelationClass         { Key XDataAssociation        _ }  -- Relation class
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
  '='                   { Key XAssign                 _ }  -- "=" operator
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
    : GraphClass argument '=' ACCESS '(' chars ')' Program  { StartExpr $2 $6 $8 }
      -- Start symbol, defines the structure of a GQL program

Program
    : Statement Program  { $1 : $2 }  -- Program consists of multiple statements
    | {- empty -}        { []      }  -- Empty program

Statement
    : Expression               { Expression $1  }  -- Expression statement
    | Conditional              { $1             }  -- If statement
    | Through                  { $1             }  -- For loop statement
    | STDOUT '(' argument ')'  { Output $3      }  -- Output statement

LiteralHelper
    : ExpressionMathXAS                                { ExpressionMathXAS $1            }  -- Mathematical expression
    | HEADER                                           { ArgumentConstructor (Object $1) }  -- Header literal
    | chars                                            { String $1                       }  -- String literal
    | RegularExpression                                { RegularExpression $1            }  -- Regular expression literal
    | argument '.' CALLDATAPOINT '(' Expression ')'    { AccessDataNode $1 $5            }  -- Get node expression

CharsHelper
    : chars                     { [$1]      }  -- Single string in the list
    | chars '-' CharsHelper     { ($1 : $3) }  -- Multiple strings in the list
        
Expression
    : ArgumentQuery             { $1                     }  -- Function application expression
    | ExpressionBool            { ExpressionBool $1      }  -- Boolean expression
    | ExpressionLink            { ExpressionLink $1      }  -- Assignment expression
    | ArgumentConstructor       { ArgumentConstructor $1 }  -- ArgumentConstructor expression
    | LiteralHelper             { $1                     }  -- Literal expression
    | '(' Expression ')'        { $2                     }  -- Parenthesized expression

ExpressionMathXAS
    : ExpressionMathDMn                              { $1                }  -- Devolve to ExpressionMathDMn
    | ExpressionMathXAS PLUS ExpressionMathXAS       { ArithmeticA $1 $3 }  -- Addition
    | ExpressionMathXAS SUBT ExpressionMathXAS       { ArithmeticS $1 $3 }  -- Subtraction

ExpressionMathDMn 
    : ExpressionMathXAS MULT ExpressionMathXAS      { ArithmeticM $1 $3 }  -- Multiplication
    | ExpressionMathXAS DIV ExpressionMathXAS       { ArithmeticD $1 $3 }  -- Division
    | n                                             { Num $1            }  -- Integer literal

ExpressionBool
    : Expression AND Expression               { BoolConjunction $1 $3  }  -- Logical AND
    | Expression OR Expression                { BoolUnion $1 $3        }  -- Logical OR
    | ExpressionBoolComparison                { $1                     }  -- Simple boolean expression

ExpressionBoolComparison
    : True                                             { Bool True                      }  -- Boolean true literal
    | False                                            { Bool False                     }  -- Boolean false literal
    | Expression 'i==' Expression                      { StrictEqualityQuery $1 $3      }  -- Equality comparison
    | Expression '!==' Expression                      { StrictInqualityQuery $1 $3     }  -- Inequality comparison
    | Expression '<' Expression                        { StrictLesserQuery $1 $3        }  -- Less than comparison
    | Expression '>' Expression                        { StrictGreaterQuery $1 $3       }  -- Greater than comparison
    | Expression '<<' Expression                       { SlackLesserQuery $1 $3         }  -- Less than or equal to comparison
    | Expression '>>' Expression                       { SlackGreaterQuery $1 $3        }  -- Greater than or equal to comparison
    | '{' ExpressionBool '}' '^' argument              { AssociationEnd $2 $5           }  -- End relation query
    | argument '{' ExpressionBool '}' '^'              { AssociationStart $1 $3         }  -- Start relation query
    | '(' ExpressionBool ')'                           { $2                             }  -- Parenthesized boolean expression
    | argument '{' ExpressionBool '}' '^' argument     { AssociationStatement $1 $3 $6  }  -- Relation query
    | Expression '.' HAS '(' CharsHelper ')'           { HasQuery $1 $5                 }  -- HasQuery expression

ExpressionLink
    : Class argument '=' Expression            { ClassArgumentStatement $1 $2 $4     }  -- Typed assignment
    | ArgumentConstructor '=' Expression       { Assign $1 $3                        }  -- Regular assignment
    | Class argument                           { Assert $1 $2                        }  -- Variable declaration
    | ArgumentConstructor '++' Expression      { ArgumentIncrement $1 $3             }  -- Increment assignment
    | ArgumentConstructor '--' Expression      { ArgumentDecrement $1 $3             }  -- Decrement assignment

ArgumentConstructor
    : argument                               { Object $1                }  -- Variable
    | argument '.' argument                  { ArgumentAttribute $1 $3  }  -- Property access
    | argument '.' HEADER                    { ArgumentAttribute $1 $3  }  -- Property access with header

ArgumentQuery
    : argument '.' CASE '(' ExpressionBool ')'                 { CaseQuery $1 $5         }  -- Match query
    | argument '.' PLUS '(' Expression ')'                     { AddQuery $1 $5          }  -- Add query
    | argument '.' CALLASSOCIATION '(' ExpressionBool ')'      { AssociationQuery $1 $5  }  -- Get relation
    | argument '.' NEGATE '(' Expression ')'                   { NegateData $1 $5        }  -- Exclusion expression

Conditional
    : CONDIF '(' ExpressionBool ')' '{' Program '}'                          { CondIfQuery $3 $6        }  -- Conditional query with if
    | CONDIF '(' ExpressionBool ')' '{' Program '}' CONDELIF '{' Program '}' { CondElifQuery $3 $6 $10  }  -- Conditional query with elif

Through  
    : THROUGH '(' Class argument ':' Expression ')' '{' Program '}'          { ThroughQuery $3 $4 $6 $9 }  -- Through query

Class
    : GraphClass         { Class $1 }  -- Graph class
    | IntegerClass       { Class $1 }  -- Integer class
    | StringClass        { Class $1 }  -- String class
    | BooleanClass       { Class $1 }  -- Boolean class
    | NodeClass          { Class $1 }  -- Node class
    | RelationClass      { Class $1 }  -- Relation class

-- Parser monad and utility functions
{

parseError :: [Token] -> a
parseError [] = error "parse error detected"
parseError (t:_) = error $ "parse error @ Ln " ++ show (getLn (getPos t)) ++ " Col " ++ show (getCol (getPos t)) ++ " on token: " ++ show t
  where
    getPos (Key _ p) = p
    getLn (AlexPn _ l _) = l  
    getCol (AlexPn _ _ c) = c

-- Abstract syntax tree data types
data Start
  = StartExpr String String Program
  deriving(Eq, Show)

type Program 
  = [Statement]

data Statement
  = Expression Expression
  | CondIfQuery ExpressionBool Program
  | CondElifQuery ExpressionBool Program Program
  | ThroughQuery Class String Expression Program
  | Output String
  deriving(Eq, Show)

data Expression
  = ExpressionMathXAS ExpressionMathXAS
  | String String
  | RegularExpression String
  | CaseQuery String ExpressionBool
  | AddQuery String Expression
  | ExpressionBool ExpressionBool
  | AssociationQuery String ExpressionBool
  | NegateData String Expression
  | AccessDataNode String Expression
  | ExpressionLink ExpressionLink
  | ArgumentConstructor ArgumentConstructor
  deriving(Eq, Show)

data ExpressionMathXAS
  = Num Int
  | ArithmeticA ExpressionMathXAS ExpressionMathXAS
  | ArithmeticS ExpressionMathXAS ExpressionMathXAS
  | ArithmeticM ExpressionMathXAS ExpressionMathXAS
  | ArithmeticD ExpressionMathXAS ExpressionMathXAS
  deriving(Eq, Show)

data ExpressionBool
  = Bool Bool
  | StrictEqualityQuery Expression Expression
  | StrictInqualityQuery Expression Expression
  | StrictLesserQuery Expression Expression
  | StrictGreaterQuery Expression Expression
  | SlackLesserQuery Expression Expression
  | SlackGreaterQuery Expression Expression
  | BoolConjunction Expression Expression
  | BoolUnion Expression Expression
  | AssociationEnd ExpressionBool String
  | AssociationStart String ExpressionBool
  | AssociationStatement String ExpressionBool String
  | HasQuery Expression [String]
  deriving(Eq, Show)

data ExpressionLink
  = ClassArgumentStatement Class String Expression
  | ArgumentIncrement ArgumentConstructor Expression
  | ArgumentDecrement ArgumentConstructor Expression
  | Assign ArgumentConstructor Expression
  | Assert Class String
  deriving(Eq, Show)

data ArgumentConstructor
  = Object String
  | ArgumentAttribute String String
  deriving(Eq, Show)

data Class
  = Class Token
  deriving(Eq, Show)

}