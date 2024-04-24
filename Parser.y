{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

-- Token definitions
%token
  n                                     { Key (KeyNum $$)                  _ }
  ACCESS                                { Key KeyACCESSToken               _ }
  CASE                                  { Key KeyCASEToken                 _ }
  STDOUT                                { Key KeySTDOUTToken               _ }
  'AND'                                 { Key KeyLogicalAnd                _ }
  'OR'                                  { Key KeyLogicalOr                 _ }
  '('                                   { Key KeyBracketLeft               _ }
  ')'                                   { Key KeyBracketRight              _ }
  DataStructure                         { Key KeyDataStructureToken        _ }
  Num                                   { Key KeyNumToken                  _ }
  Chars                                 { Key KeyCharsToken                _ }
  Bool                                  { Key KeyBoolToken                 _ }
  '{'                                   { Key KeyBraceLeft                 _ }
  '}'                                   { Key KeyBraceRight                _ }
  CONDIF                                { Key KeyCONDIFToken               _ }
  CONDELIF                              { Key KeyCONDELIFToken             _ }
  THROUGH                               { Key KeyTHROUGHToken              _ }
  ':'                                   { Key KeySeparatorColon            _ }
  identity                              { Key (KeyIdentity $$)             _ }
  '>='                                  { Key KeyInequalitySlackGreater    _ }
  '<='                                  { Key KeyInequalitySlackLesser     _ }
  '>'                                   { Key KeyInequalityStrictGreater   _ }
  '<'                                   { Key KeyInequalityStrictLesser    _ }
  '='                                   { Key KeySet                       _ }
  'i=='                                 { Key KeyIdentical                 _ }
  '['                                   { Key KeyBracketLeftSquare         _ }
  ']'                                   { Key KeyBracketRightSquare        _ }
  '->'                                  { Key KeyDirectionalRight          _ }
  '-'                                   { Key KeyNumericMinus              _ }
  regular                               { Key (KeyRegular $$)              _ }
  PLUS                                  { Key KeyPlusToken                 _ }
  '.'                                   { Key KeyPeriod                    _ }
  header                                { Key (KeyHeader $$)               _ }
  chars                                 { Key (KeyChars $$)                _ }
  True                                  { Key KeyBoolTrue                  _ }
  False                                 { Key KeyBoolFalse                 _ }
  ';'                                   { Key KeySeparatorColonSemi        _ }
  '!=='                                 { Key KeyIdenticalNot              _ }
  ','                                   { Key KeySeparatorComma            _ }
  DataPoint                             { Key KeyDataPointToken            _ }
  Association                           { Key KeyAssociationToken          _ }
  CALLASSOCIATION                       { Key KeyCallAssociationToken      _ }
  HAS                                   { Key KeyHasToken                  _ }
  CALLDATAPOINT                         { Key KeyCallDataPointToken        _ }
  '+'                                   { Key KeyNumericAdd                _ }
  '*'                                   { Key KeyNumericMultiply           _ }
  '/'                                   { Key KeyNumericDivide             _ }
  '=+'                                  { Key KeyNumericIncrease           _ }
  '=-'                                  { Key KeyNumericDecrease           _ }
  NOT                                   { Key KeyNotToken                  _ }

-- Operator precedence
%right '=' '=+' '=-'
%left ','
%left ':'
%right 'OR'
%right 'AND'
%nonassoc 'i==' '!=='
%nonassoc '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/'
%left '.'

%%

-- Grammar rules

-- QQ
QQ
  : Q QQ                                { ($1 : $2) }
  | {- empty -}                         { []        }

-- Q
Q
  : X ';'                               { X $1 }
  | CONDIFQ                             { $1   }
  | THROUGHQ                            { $1   }

-- X
X
  : IsolateXX                           { $1        }
  | StatementXX                         { $1        }
  | StatementTryXX                      { $1        }
  | BoolXX       %shift                 { BoolXX $1 }
  | SetXX                               { $1        }
  | IdentifyXX                          { $1        }
  | '(' X ')'                           { $2        }

-- Identify XX
IdentifyXX
  : identity                            { Identifier $1 }
  | NumericXX                           { NumericXX $1  }
  | header                              { Identifier $1 }
  | chars                               { Chars $1      }
  | regular                             { Regular $1    }

-- Set XX
SetXX
  : Class identity '=' X                { ClassFinalSet $1 $2 $4 }
  | X '=' X                             { Set $1 $3              }
  | Class identity               %shift { ClassShow $1 $2        }
  | X '=+' X                            { NumericIncrease $1 $3  }
  | X '=-' X                            { NumericDecrease $1 $3  }
  | X '.' CALLDATAPOINT '(' X ')'       { CallDataPoint $1 $5    }

-- Isolate XX
IsolateXX
  : X '.' identity                      { CallAttribute $1 $3 }
  | X '.' header                        { CallAttribute $1 $3 }

-- Statement XX
StatementXX
  : ACCESS '(' chars ')'                { ACCESS $3 }
  | STDOUT '(' identity ')'             { STDOUT $3 }

-- Statement Try XX
StatementTryXX
  : X '.' CASE '(' BoolXX ')'            { CASEQ $1 $5           }
  | X '.' PLUS '(' X ')'                 { PlusQ $1 $5           }
  | X '.' CALLASSOCIATION '(' BoolXX ')' { CallAssociation $1 $5 }
  | X '.' NOT '(' X ')'                  { Not $1 $5             }

-- Numeric XX
NumericXX
  : NumericQ                            { $1                    }
  | NumericXX '+' NumericXX             { PlusPlus $1 $3        }
  | NumericXX '-' NumericXX             { NumericSubtract $1 $3 }

NumericQ
  : NumericXX '*' NumericXX             { NumericMultiply $1 $3 }
  | NumericXX '/' NumericXX             { NumericDivide $1 $3   }
  | n                                   { IntTerminal $1        }

-- Bool XX
BoolXX
  : LogicalBoolXX 'AND' BoolXX          { LogicalAnd $1 $3 }
  | LogicalBoolXX 'OR' BoolXX           { LogicalOr $1 $3  }
  | LogicalBoolXX     %shift            { $1               }

LogicalBoolXX
  : True                                      { BoolTerminal True             }
  | False                                     { BoolTerminal False            }
  | X 'i==' X                                 { Identical $1 $3               }
  | X '!==' X                                 { IdenticalNot $1 $3            }
  | X '<' X                                   { InequalityStrictLesser $1 $3  }
  | X '>' X                                   { InequalityStrictGreater $1 $3 }
  | X '<=' X                                  { InequalitySlackLesser $1 $3   }
  | X '>=' X                                  { InequalitySlackGreater $1 $3  }
  | '-' '[' BoolXX ']' '->' identity          { AssociationEndQ $3 $6         }
  | identity '-' '[' BoolXX ']' '->'          { AssociationStartQ $1 $4       }
  | '(' BoolXX ')'                            { $2                            }
  | identity '-' '[' BoolXX ']' '->' identity { AssociationQ $1 $4 $7         }
  | X '.' HAS '(' CharsQ ')'                  { Has $1 $5                     }

-- Conditional Q
CONDIFQ
  : CONDIF '(' BoolXX ')' '{' QQ '}'                          { CONDIFQ $3 $6        }
  | CONDIF '(' BoolXX ')' '{' QQ '}' CONDELIF '{' QQ '}'      { CONDELIFQ $3 $6 $10  }

-- THROUGH Q
THROUGHQ
  : THROUGH '(' Class identity ':' X ')' '{' QQ '}'           { THROUGHQ $3 $4 $6 $9 }

-- Chars Q
CharsQ
  : chars                               { [$1]      }
  | chars ',' CharsQ                    { ($1 : $3) }

-- Class
Class
  : DataStructure                       { ClassGen $1 }
  | Num                                 { ClassGen $1 }
  | Chars                               { ClassGen $1 }
  | Bool                                { ClassGen $1 }
  | DataPoint                           { ClassGen $1 }
  | Association                         { ClassGen $1 }

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
  | AssociationEndQ BoolXX String
  | AssociationStartQ String BoolXX
  | AssociationQ String BoolXX String
  | Has X [String]
  deriving (Eq, Show)

data Class
  = ClassGen Token
  deriving (Eq, Show)

}