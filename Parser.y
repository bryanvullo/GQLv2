{
module Parser where
import Lexer
}

%name parser
%tokentype { Token      }
%error     { parseError }

-- Token declarations
%token
  n                     { Key (XNum $$)               _ }  -- Number literal, e.g., 42
  chars                 { Key (XChar $$)              _ }  -- Character literal, e.g., "hello"
  RegularExpression     { Key (XRegularExpression $$) _ }  -- Regular expression literal, e.g., /[a-z]+/
  argument              { Key (XArgument $$)          _ }  -- Argument, e.g., person
  HEADER                { Key (XHeader $$)            _ }  -- Header, e.g., :LABEL
  True                  { Key XBooleanTrue            _ }  -- "True" boolean literal
  False                 { Key XBooleanFalse           _ }  -- "False" boolean literal
  GraphClass            { Key XDataGraph              _ }  -- Graph class, e.g., DataGraph network
  IntegerClass          { Key XInt                    _ }  -- Integer class, e.g., Integer age
  StringClass           { Key XStr                    _ }  -- String class, e.g., String name
  BooleanClass          { Key XBool                   _ }  -- Boolean class, e.g., Boolean isActive
  NodeClass             { Key XNode                   _ }  -- Node class, e.g., Node person
  RelationClass         { Key XDataAssociation        _ }  -- Relation class, e.g., Association friendship
  ACCESS                { Key XACCESS                 _ }  -- "ACCESS" keyword, e.g., ACCESS("file.n4j")
  CASE                  { Key XCASE                   _ }  -- "CASE" keyword, e.g., CASE(condition)
  STDOUT                { Key XSTDOUT                 _ }  -- "STDOUT" keyword, e.g., STDOUT(result)
  HAS                   { Key XHAS                    _ }  -- "HAS" keyword, e.g., person.HAS("name")
  CONDIF                { Key XCONDIF                 _ }  -- "CONDIF" keyword, e.g., CONDIF(condition)
  CONDELIF              { Key XCONDELIF               _ }  -- "CONDELIF" keyword, e.g., CONDELIF(condition)
  THROUGH               { Key XTHROUGH                _ }  -- "THROUGH" keyword, e.g., THROUGH(Node person : graph)
  NEGATE                { Key XNegate                 _ }  -- "NEGATE" keyword, e.g., NEGATE(expression)
  CALLASSOCIATION       { Key XAssociationCheck       _ }  -- "CALLASSOCIATION" keyword, e.g., node.CALLASSOCIATION(condition)
  CALLDATAPOINT         { Key XDataNodeCheck          _ }  -- "CALLDATAPOINT" keyword, e.g., node.CALLDATAPOINT(expression)
  AND                   { Key XLogicalAND             _ }  -- "AND" logical operator, e.g., condition1 AND condition2
  OR                    { Key XLogicalOR              _ }  -- "OR" logical operator, e.g., condition1 OR condition2
  PLUS                  { Key XPlus                   _ }  -- "+" operator, e.g., graph.PLUS(node)
  SUBT                  { Key XSubtract               _ }  -- "-" operator, e.g., a - b
  MULT                  { Key XMultiply               _ }  -- "*" operator, e.g., a * b
  DIV                   { Key XDivide                 _ }  -- "/" operator, e.g., a / b
  '('                   { Key XBracketLeft            _ }  -- Left parenthesis, e.g., (expression)
  ')'                   { Key XBracketRight           _ }  -- Right parenthesis, e.g., (expression)
  '>>'                  { Key XSlackGreater           _ }  -- ">>" operator, e.g., a >> b
  '<<'                  { Key XSlackLesser            _ }  -- "<<" operator, e.g., a << b
  '>'                   { Key XStrictGreater          _ }  -- ">" operator, e.g., a > b
  '<'                   { Key XStrictLesser           _ }  -- "<" operator, e.g., a < b
  '='                   { Key XAssign                 _ }  -- "=" operator, e.g., a = b
  'i=='                 { Key XLogicalEquation        _ }  -- "i==" equality operator, e.g., a i== b
  '!=='                 { Key XLogicalInequation      _ }  -- "!==" inequality operator, e.g., a !== b
  '.'                   { Key XPeriod                 _ }  -- "." operator, e.g., node.property
  '-'                   { Key XHyphen                 _ }  -- "-" operator, e.g., a - b
  '^'                   { Key XAssociationSymbol      _ }  -- "^" operator, e.g., a{condition}^b
  '{'                   { Key XBraceLeft              _ }  -- Left brace, e.g., {expression}
  '}'                   { Key XBraceRight             _ }  -- Right brace, e.g., {expression}
  ':'                   { Key XColonSymbol            _ }  -- ":" symbol, e.g., Node person : graph
  '++'                  { Key XNumericalIncrement     _ }  -- "++" increment operator, e.g., a++
  '--'                  { Key XNumericalDecrement     _ }  -- "--" decrement operator, e.g., a--

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
      -- e.g., DataGraph network = ACCESS("file.n4j") { ... }

Program
    : Statement Program  { $1 : $2 }  -- Program consists of multiple statements
    | {- empty -}        { []      }  -- Empty program

Statement
    : Expression               { Expression $1  }  -- Expression statement
    | Conditional              { $1             }  -- If statement
    | Through                  { $1             }  -- For loop statement
    | STDOUT '(' argument ')'  { Output $3      }  -- Output statement, e.g., STDOUT(result)

LiteralHelper
    : ExpressionMathXAS                                { ExpressionMathXAS $1            }  -- Mathematical expression
    | HEADER                                           { ArgumentConstructor (Object $1) }  -- Header literal, e.g., :LABEL
    | chars                                            { String $1                       }  -- String literal, e.g., "hello"
    | RegularExpression                                { RegularExpression $1            }  -- Regular expression literal, e.g., /[a-z]+/
    | argument '.' CALLDATAPOINT '(' Expression ')'    { AccessDataNode $1 $5            }  -- Get node expression, e.g., node.CALLDATAPOINT(expression)

CharsHelper
    : chars                     { [$1]      }  -- Single string in the list
    | chars '-' CharsHelper     { ($1 : $3) }  -- Multiple strings in the list, e.g., "a"-"b"-"c"
        
Expression
    : ArgumentQuery             { $1                     }  -- Function application expression
    | ExpressionBool            { ExpressionBool $1      }  -- Boolean expression
    | ExpressionLink            { ExpressionLink $1      }  -- Assignment expression
    | ArgumentConstructor       { ArgumentConstructor $1 }  -- ArgumentConstructor expression
    | LiteralHelper             { $1                     }  -- Literal expression
    | '(' Expression ')'        { $2                     }  -- Parenthesized expression, e.g., (a + b)

ExpressionMathXAS
    : ExpressionMathDMn                              { $1                }  -- Devolve to ExpressionMathDMn
    | ExpressionMathXAS PLUS ExpressionMathXAS       { ArithmeticA $1 $3 }  -- Addition, e.g., a + b
    | ExpressionMathXAS SUBT ExpressionMathXAS       { ArithmeticS $1 $3 }  -- Subtraction, e.g., a - b

ExpressionMathDMn 
    : ExpressionMathXAS MULT ExpressionMathXAS      { ArithmeticM $1 $3 }  -- Multiplication, e.g., a * b
    | ExpressionMathXAS DIV ExpressionMathXAS       { ArithmeticD $1 $3 }  -- Division, e.g., a / b
    | n                                             { Num $1            }  -- Integer literal, e.g., 42

ExpressionBool
    : ExpressionBool AND ExpressionBool               { BoolConjunction $1 $3  }  -- Logical AND, e.g., condition1 AND condition2
    | ExpressionBool OR ExpressionBool                { BoolUnion $1 $3        }  -- Logical OR, e.g., condition1 OR condition2
    | ExpressionBoolComparison                        { $1                     }  -- Simple boolean expression

ExpressionBoolComparison
    : True                                             { Bool True                      }  -- Boolean true literal
    | False                                            { Bool False                     }  -- Boolean false literal
    | Expression 'i==' Expression                      { StrictEqualityQuery $1 $3      }  -- Equality comparison, e.g., a i== b
    | Expression '!==' Expression                      { StrictInqualityQuery $1 $3     }  -- Inequality comparison, e.g., a !== b
    | Expression '<' Expression                        { StrictLesserQuery $1 $3        }  -- Less than comparison, e.g., a < b
    | Expression '>' Expression                        { StrictGreaterQuery $1 $3       }  -- Greater than comparison, e.g., a > b
    | Expression '<<' Expression                       { SlackLesserQuery $1 $3         }  -- Less than or equal to comparison, e.g., a << b
    | Expression '>>' Expression                       { SlackGreaterQuery $1 $3        }  -- Greater than or equal to comparison, e.g., a >> b
    | '{' ExpressionBool '}' '^' argument              { AssociationEnd $2 $5           }  -- End relation query, e.g., {condition}^node
    | argument '{' ExpressionBool '}' '^'              { AssociationStart $1 $3         }  -- Start relation query, e.g., node{condition}^
    | '(' ExpressionBool ')'                           { $2                             }  -- Parenthesized boolean expression, e.g., (a AND b)
    | argument '{' ExpressionBool '}' '^' argument     { AssociationStatement $1 $3 $6  }  -- Relation query, e.g., node1{condition}^node2
    | Expression '.' HAS '(' CharsHelper ')'           { HasQuery $1 $5                 }  -- HasQuery expression, e.g., node.HAS("property")

ExpressionLink
    : Class argument '=' Expression            { ClassArgumentStatement $1 $2 $4     }  -- Typed assignment, e.g., Integer age = 25
    | ArgumentConstructor '=' Expression       { Assign $1 $3                        }  -- Regular assignment, e.g., name = "John"
    | Class argument                           { Assert $1 $2                        }  -- Variable declaration, e.g., String name
    | ArgumentConstructor '++' Expression      { ArgumentIncrement $1 $3             }  -- Increment assignment, e.g., age++
    | ArgumentConstructor '--' Expression      { ArgumentDecrement $1 $3             }  -- Decrement assignment, e.g., age--

ArgumentConstructor
    : argument                               { Object $1                }  -- Variable, e.g., person
    | argument '.' argument                  { ArgumentAttribute $1 $3  }  -- Property access, e.g., person.name
    | argument '.' HEADER                    { ArgumentAttribute $1 $3  }  -- Property access with header, e.g., person.:LABEL

ArgumentQuery
    : argument '.' CASE '(' ExpressionBool ')'                 { CaseQuery $1 $5         }  -- Match query, e.g., node.CASE(condition)
    | argument '.' PLUS '(' Expression ')'                     { AddQuery $1 $5          }  -- Add query, e.g., graph.PLUS(node)
    | argument '.' CALLASSOCIATION '(' ExpressionBool ')'      { AssociationQuery $1 $5  }  -- Get relation, e.g., node.CALLASSOCIATION(condition)
    | argument '.' NEGATE '(' ExpressionBool ')'                   { NegateData $1 $5        }  -- Exclusion expression, e.g., graph.NEGATE(expression)

Conditional
    : CONDIF '(' ExpressionBool ')' '{' Program '}'                          { CondIfQuery $3 $6        }  -- Conditional query with if, e.g., CONDIF(condition) { ... }
    | CONDIF '(' ExpressionBool ')' '{' Program '}' CONDELIF '{' Program '}' { CondElifQuery $3 $6 $10  }  -- Conditional query with elif, e.g., CONDIF(condition1) { ... } CONDELIF(condition2) { ... }

Through
    : THROUGH '(' Class argument ':' Expression ')' '{' Program '}'          { ThroughQuery $3 $4 $6 $9 }  -- Through query, e.g., THROUGH(Node person : graph) { ... }

Class
    : GraphClass         { GraphClass }  -- Graph class, e.g., DataGraph
    | IntegerClass       { IntegerClass }  -- Integer class, e.g., Integer
    | StringClass        { StringClass }  -- String class, e.g., String
    | BooleanClass       { BooleanClass }  -- Boolean class, e.g., Boolean
    | NodeClass          { NodeClass }  -- Node class, e.g., Node
    | RelationClass      { RelationClass }  -- Relation class, e.g., Association

-- Parser monad and utility functions
{

parseError :: [Token] -> a
parseError [] = error "parse error detected"
parseError (t:_) = error $ "parse error @ Ln:Col " ++ show (getLn (getPos t)) ++ ":" ++ show (getCol (getPos t)) ++ ", on token: " ++ show t
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
    | NegateData String ExpressionBool
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
    | BoolConjunction ExpressionBool ExpressionBool
    | BoolUnion ExpressionBool ExpressionBool
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
    = GraphClass 
    | IntegerClass
    | StringClass
    | BooleanClass
    | NodeClass
    | RelationClass
    deriving(Eq, Show)

}