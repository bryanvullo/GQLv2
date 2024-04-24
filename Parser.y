{
module Parser where
import Lexer  
}

%name parser
%tokentype { Token }
%error { parseError }

-- Token definitions, mapping tokens to Haskell data constructors
%token
  n                                     { Key (KeyNum $$)                  _ } -- Integer literal
  ACCESS                                { Key KeyACCESSToken               _ } -- Keyword for accessing input files 
  CASE                                  { Key KeyCASEToken                 _ } -- Keyword for case expressions
  STDOUT                                { Key KeySTDOUTToken               _ } -- Keyword for standard output
  'AND'                                 { Key KeyLogicalAnd                _ } -- Logical AND operator
  'OR'                                  { Key KeyLogicalOr                 _ } -- Logical OR operator
  'NEGATE'                              { Key KeyLogicalNegate             _ } -- Logical negation operator (?)
  '('                                   { Key KeyBracketLeft               _ } -- Left parenthesis
  ')'                                   { Key KeyBracketRight              _ } -- Right parenthesis
  DataStructure                         { Key KeyDataStructureToken        _ } -- Keyword for the Graph data structure
  Num                                   { Key KeyNumToken                  _ } -- Keyword for numeric type
  Chars                                 { Key KeyCharsToken                _ } -- Keyword for string type
  Bool                                  { Key KeyBoolToken                 _ } -- Keyword for boolean type
  '{'                                   { Key KeyBraceLeft                 _ } -- Left brace
  '}'                                   { Key KeyBraceRight                _ } -- Right brace
  CONDIF                                { Key KeyCONDIFToken               _ } -- Keyword for conditional if expressions
  CONDELIF                              { Key KeyCONDELIFToken             _ } -- Keyword for conditional else-if expressions  
  THROUGH                               { Key KeyTHROUGHToken              _ } -- Keyword for loops over data
  ':'                                   { Key KeySeparatorColon            _ } -- Colon separator 
  identity                              { Key (KeyIdentity $$)             _ } -- Identifier (variable name)  
  '>='                                  { Key KeyInequalitySlackGreater    _ } -- Greater than or equal operator
  '<='                                  { Key KeyInequalitySlackLesser     _ } -- Less than or equal operator
  '>'                                   { Key KeyInequalityStrictGreater   _ } -- Strictly greater than operator
  '<'                                   { Key KeyInequalityStrictLesser    _ } -- Strictly less than operator
  '='                                   { Key KeySet                       _ } -- Variable assignment operator
  'i=='                                 { Key KeyIdentical                 _ } -- Strict equality test operator  
  '['                                   { Key KeyBracketLeftSquare         _ } -- Left square bracket
  ']'                                   { Key KeyBracketRightSquare        _ } -- Right square bracket
  '>>'                                  { Key KeyDirectionalRight          _ } -- Right arrow for defining edges  
  '-'                                   { Key KeyNumericMinus              _ } -- Subtraction operator
  regular                               { Key (KeyRegular $$)              _ } -- Regular expression literal
  PLUS                                  { Key KeyPlusToken                 _ } -- Keyword for adding nodes/edges to output
  '.'                                   { Key KeyPeriod                    _ } -- Period for attribute access
  header                                { Key (KeyHeader $$)               _ } -- Header name literal  
  chars                                 { Key (KeyChars $$)                _ } -- String literal
  True                                  { Key KeyBoolTrue                  _ } -- Boolean literal "True"  
  False                                 { Key KeyBoolFalse                 _ } -- Boolean literal "False"
  ';'                                   { Key KeySeparatorColonSemi        _ } -- Semicolon separator
  '!=='                                 { Key KeyIdenticalNot              _ } -- Strict inequality test operator
  ','                                   { Key KeySeparatorComma            _ } -- Comma separator  
  DataPoint                             { Key KeyDataPointToken            _ } -- Keyword for nodes in the graph
  Association                           { Key KeyAssociationToken          _ } -- Keyword for edges in the graph 
  CALLASSOCIATION                       { Key KeyCallAssociationToken      _ } -- Keyword for querying edges
  HAS                                   { Key KeyHasToken                  _ } -- Keyword for property queries on nodes/edges 
  CALLDATAPOINT                         { Key KeyCallDataPointToken        _ } -- Keyword for querying nodes  
  '+'                                   { Key KeyNumericPlus               _ } -- Addition operator
  '*'                                   { Key KeyNumericMultiply           _ } -- Multiplication operator
  '/'                                   { Key KeyNumericDivide             _ } -- Division operator
  '=+'                                  { Key KeyNumericIncrease           _ } -- Increment operator  
  '=-'                                  { Key KeyNumericDecrease           _ } -- Decrement operator
  NOT                                   { Key KeyNotToken                  _ } -- Logical negation operator (?)

-- Operator precedence and associativity rules  
%right '=' '=+' '=-'  
%left ','
%left ':'
%right 'OR'
%right 'AND'
%right 'NEGATE'
%nonassoc 'i==' '!==' 
%nonassoc '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/'  
%left '.'

%%

-- Top-level program is a sequence of statements
QQ
  : Q QQ                                { ($1 : $2) }  -- Sequence of statements
  | {- empty -}                         { []        }  -- Empty program

-- Statement 
Q
  : X ';'                               { X $1 }       -- Expression statement
  | CONDIFQ                             { $1   }       -- Conditional if statement
  | THROUGHQ                            { $1   }       -- Loop over data  

-- Expression  
X  
  : IsolateXX                           { $1        }  -- Attribute access expression
  | StatementXX                         { $1        }  -- Input/output statement expression
  | StatementTryXX                      { $1        }  -- Graph traversal/filtering expression
  | BoolXX       %shift                 { BoolXX $1 }  -- Boolean expression 
  | SetXX                               { $1        }  -- Variable declaration/assignment expression
  | IdentifyXX                          { $1        }  -- Identifier or literal expression
  | '(' X ')'                           { $2        }  -- Parenthesized expression

-- Identifiers and literals  
IdentifyXX
  : identity                            { Identifier $1 }  -- Variable identifier  
  | NumericXX                           { NumericXX $1  }  -- Numeric expression
  | header                              { Identifier $1 }  -- Header name identifier
  | chars                               { Chars $1      }  -- String literal
  | regular                             { Regular $1    }  -- Regular expression literal

-- Variable declarations and assignments
SetXX  
  : Class identity '=' X                { ClassFinalSet $1 $2 $4 }  -- Typed variable declaration with initialization
  | X '=' X                             { Set $1 $3              }  -- Variable assignment
  | Class identity               %shift { ClassShow $1 $2        }  -- Typed variable declaration  
  | X '=+' X                            { NumericIncrease $1 $3  }  -- Increment assignment
  | X '=-' X                            { NumericDecrease $1 $3  }  -- Decrement assignment
  | X '.' CALLDATAPOINT '(' X ')'       { CallDataPoint $1 $5    }  -- Node query expression

-- Attribute access  
IsolateXX
  : X '.' identity                      { CallAttribute $1 $3 }  -- Attribute access by identifier  
  | X '.' header                        { CallAttribute $1 $3 }  -- Attribute access by header name

-- Input/output statements
StatementXX  
  : ACCESS '(' chars ')'                { ACCESS $3 }  -- File input statement
  | STDOUT '(' identity ')'             { STDOUT $3 }  -- Console output statement

-- Graph traversal and filtering  
StatementTryXX
  : X '.' CASE '(' BoolXX ')'            { CASEQ $1 $5           }  -- Case expression  
  | X '.' PLUS '(' X ')'                 { PlusQ $1 $5           }  -- Node/edge addition expression
  | X '.' CALLASSOCIATION '(' BoolXX ')' { CallAssociation $1 $5 }  -- Edge query expression
  | X '.' NOT '(' X ')'                  { Not $1 $5             }  -- Logical negation expression  

-- Numeric expressions
NumericXX
  : NumericQ                            { $1                    }  -- Numeric expression (product/quotient)  
  | NumericXX '+' NumericXX             { PlusPlus $1 $3        }  -- Addition expression
  | NumericXX '-' NumericXX             { NumericSubtract $1 $3 }  -- Subtraction expression

NumericQ  
  : NumericXX '*' NumericXX             { NumericMultiply $1 $3 }  -- Multiplication expression
  | NumericXX '/' NumericXX             { NumericDivide $1 $3   }  -- Division expression
  | n                                   { IntTerminal $1        }  -- Integer literal

-- Boolean expressions  
BoolXX
  : LogicalBoolXX 'AND' BoolXX          { LogicalAnd $1 $3 }      -- Logical AND expression
  | LogicalBoolXX 'OR' BoolXX           { LogicalOr $1 $3  }      -- Logical OR expression
  | 'NEGATE' BoolXX                     { LogicalNegate $2 }      -- Logical negation expression  
  | LogicalBoolXX     %shift            { $1               }      -- Boolean term expression

LogicalBoolXX
  : True                                      { BoolTerminal True             }  -- Boolean literal "True"  
  | False                                     { BoolTerminal False            }  -- Boolean literal "False"
  | X 'i==' X                                 { Identical $1 $3               }  -- Strict equality expression  
  | X '!==' X                                 { IdenticalNot $1 $3            }  -- Strict inequality expression
  | X '<' X                                   { InequalityStrictLesser $1 $3  }  -- Strict less than expression
  | X '>' X                                   { InequalityStrictGreater $1 $3 }  -- Strict greater than expression
  | X '<=' X                                  { InequalitySlackLesser $1 $3   }  -- Less than or equal expression
  | X '>=' X                                  { InequalitySlackGreater $1 $3  }  -- Greater than or equal expression
  | '-' '[' BoolXX ']' '>>' identity          { AssociationEndQ $3 $6         }  -- Edge target query expression
  | identity '-' '[' BoolXX ']' '>>'          { AssociationStartQ $1 $4       }  -- Edge source query expression
  | '(' BoolXX ')'                            { $2                            }  -- Parenthesized boolean expression 
  | identity '-' '[' BoolXX ']' '>>' identity { AssociationQ $1 $4 $7         }  -- Edge query expression
  | X '.' HAS '(' CharsQ ')'                  { Has $1 $5                     }  -- Node/edge property query expression  

-- Conditional expressions 
CONDIFQ
  : CONDIF '(' BoolXX ')' '{' QQ '}'                          { CONDIFQ $3 $6        }  -- Conditional if expression  
  | CONDIF '(' BoolXX ')' '{' QQ '}' CONDELIF '{' QQ '}'      { CONDELIFQ $3 $6 $10  }  -- Conditional if-else expression

-- Loops over data
THROUGHQ  
  : THROUGH '(' Class identity ':' X ')' '{' QQ '}'           { THROUGHQ $3 $4 $6 $9 }  -- Loop over data

-- Comma-separated string literals  
CharsQ
  : chars                               { [$1]      }       -- Single string literal  
  | chars ',' CharsQ                    { ($1 : $3) }       -- Sequence of string literals

-- Type annotations
Class  
  : DataStructure                       { ClassGen $1 }     -- Graph data structure type
  | Num                                 { ClassGen $1 }     -- Numeric type  
  | Chars                               { ClassGen $1 }     -- String type
  | Bool                                { ClassGen $1 }     -- Boolean type
  | DataPoint                           { ClassGen $1 }     -- Node type
  | Association                         { ClassGen $1 }     -- Edge type  

{

parseError :: [Token] -> a  
parseError [] = error "Parse error at end of input\n"
parseError (Key t (AlexPn _ x y) : _) = error $ "Error " ++ show t ++ ", see " ++ show x ++ ":" ++ show y ++ "\n"

type QQ = [Q]

data Q
  = X X  
  | CONDIFQ BoolXX QQ
  | CONDELIFQ BoolXX QQ QQ  
  | THROUGHQ Class String X QQ
  deriving (Eq, Show)  

data X
  = ClassFinalSet Class String X
  | Set X X  
  | ClassShow Class String
  | Identifier String
  | NumericXX NumericXX
  | Chars String
  | Regular String
  | CASEQ X BoolXX  
  | PlusQ X X
  | ACCESS String  
  | STDOUT String
  | BoolXX BoolXX
  | CallAttribute X String  
  | CallAssociation X BoolXX
  | NumericIncrease X X
  | NumericDecrease X X
  | Not X X  
  | CallDataPoint X X
  deriving (Eq, Show)

data NumericXX  
  = IntTerminal Int
  | PlusPlus NumericXX NumericXX
  | NumericSubtract NumericXX NumericXX
  | NumericMultiply NumericXX NumericXX  
  | NumericDivide NumericXX NumericXX
  deriving (Eq, Show)  

data BoolXX
  = BoolTerminal Bool  
  | Identical X X
  | IdenticalNot X X
  | InequalityStrictLesser X X
  | InequalityStrictGreater X X
  | InequalitySlackLesser X X  
  | InequalitySlackGreater X X
  | LogicalAnd BoolXX BoolXX
  | LogicalOr BoolXX BoolXX
  | LogicalNegate BoolXX  
  | AssociationEndQ BoolXX String
  | AssociationStartQ String BoolXX  
  | AssociationQ String BoolXX String
  | Has X [String]  
  deriving (Eq, Show)

data Class  
  = ClassGen Token
  deriving (Eq, Show)  

}