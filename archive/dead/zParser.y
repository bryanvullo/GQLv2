{
module Parser where
import Lexer
}
%name parser
%tokentype { Token }
%error { parseError }

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
  '{'                                   { Key KeyBraceLeft                 _ }
  '}'                                   { Key KeyBraceRight                _ }
  CONDIF                                { Key KeyCONDIFToken               _ }
  THROUGH                               { Key KeyTHROUGHToken              _ }
  identity                              { Key (KeyIdentity $$)             _ }
  '<='                                  { Key KeyInequalitySlackLesser     _ }
  '='                                   { Key KeySet                       _ }
  '.'                                   { Key KeyPeriod                    _ }
  regular                               { Key (KeyRegular $$)              _ }
  PLUS                                  { Key KeyPlusToken                 _ }
  header                                { Key (KeyHeader $$)               _ }
  chars                                 { Key (KeyChars $$)                _ }
  True                                  { Key KeyBoolTrue                  _ }
  False                                 { Key KeyBoolFalse                 _ }
  ';'                                   { Key KeySeparatorColonSemi        _ }
  '!=='                                 { Key KeyIdenticalNot              _ }
  HAS                                   { Key KeyHasToken                  _ }
  CALLASSOCIATION                       { Key KeyCallAssociationToken      _ }
  CALLDATAPOINT                         { Key KeyCallDataPointToken        _ }
  NOT                                   { Key KeyNotToken                  _ }
  '+'                                   { Key KeyNumericPlus               _ }
  '-'                                   { Key KeyNumericMinus              _ }
  '>='                                  { Key KeyInequalitySlackGreater    _ }
  DataPoint                             { Key KeyDataPointToken            _ }
  '>>'                                  { Key KeyDirection                 _ }
  '['                                   { Key KeyBracketLeftSquare         _ }
  ']'                                   { Key KeyBracketRightSquare        _ }
  ','                                   { Key KeySeparatorComma                     _ }
  ':'                                   { Key KeySeparatorColon                     _ }

%right '='
%left 'OR'
%left 'AND'
%nonassoc '!==' '<=' '>='
%left '+' '-'
%left '.'
%%

Program
  : Statement Program               { ($1 : $2) }
  | {- empty -}                     { [] }

Statement
  : Expression ';'                                                 { X $1        }
  | CONDIF '(' BoolExpression ')' '{' Program '}'                  { CONDIFQ $3 $6 }
  | THROUGH '(' Class identity ':' Expression ')' '{' Program '}'  { THROUGHQ $3 $4 $6 $9 }

Expression
  : DataStructure identity '=' ACCESS '(' chars ')'             { ClassFinalSet $1 $2 (ACCESS $6)        }
  | identity '=' Expression                                     { Set (Identifier $1) $3                 }
  | DataStructure identity                                      { ClassShow $1 $2                        }
  | Expression '.' CASE '(' BoolExpression ')'                  { CASEQ $1 $5                            }
  | STDOUT '(' identity ')'                                     { STDOUT $3                              }
  | Expression '.' PLUS '(' Expression ')'                      { PlusQ $1 $5                            }
  | Expression '.' CALLASSOCIATION '(' BoolExpression ')'       { CallAssociation $1 $5                  }
  | Expression '.' CALLDATAPOINT '(' BoolExpression ')'         { CallDataPoint $1 $5                    }
  | Expression '.' HAS '(' CharsQ ')'                           { Has $1 $5                              }
  | Identifier                                                  { Identifier $1                          }
  | Chars                                                       { Chars $1                               }
  | BoolLiteral                                                 { BoolXX $1                              }
  | Expression '+' Expression                                   { PlusPlus $1 $3                         }

BoolExpression
  : BoolExpression 'AND' BoolExpression                         { LogicalAnd $1 $3                       }
  | BoolExpression 'OR' BoolExpression                          { LogicalOr $1 $3                        }
  | '(' BoolExpression ')'                                      { $2                                     }
  | Expression '!==' Expression                                 { IdenticalNot $1 $3                     }
  | Expression '<=' Expression                                  { InequalitySlackLesser $1 $3            }
  | Expression '>=' Expression                                  { InequalitySlackGreater $1 $3           }
  | BoolLiteral                                                 { BoolTerminal $1                        }
  | NOT '(' BoolExpression ')'                                  { LogicalNegate $3                       }
  | identity '[' BoolExpression ']' '-' '>>' identity           { AssociationQ $1 $3 $7                  }

CharsQ
  : chars                                                       { [$1]      }
  | chars ',' CharsQ                                            { $1 : $3   }

Class
  : DataStructure                                               { Graph  }
  | DataPoint                                                   { Node   }

BoolLiteral
  : True                                                        { True   }
  | False                                                       { False  }

Identifier
  : identity                                                    { $1 }

Chars
  : chars                                                       { $1 }

{

parseError :: [Token] -> a
parseError [] = error "Parse error at end of input\n"
parseError (Key t (AlexPn _ x y) : _) = error $ "Error " ++ show t ++ ", see " ++ show x ++ ":" ++ show y ++ "\n"

data Program = Program [Statement]
  deriving (Eq, Show)

data Statement
  = X Expression
  | CONDIFQ BoolExpression Program
  | THROUGHQ Class String Expression Program
  deriving (Eq, Show)

data Expression
  = ClassFinalSet Class String Expression
  | Set Expression Expression
  | ClassShow Class String
  | CASEQ Expression BoolExpression
  | STDOUT String
  | PlusQ Expression Expression
  | CallAssociation Expression BoolExpression
  | CallDataPoint Expression BoolExpression
  | Has Expression [String]
  | Identifier String
  | Chars String
  | BoolXX BoolLiteral
  | PlusPlus Expression Expression
  | ACCESS String
  deriving (Eq, Show)

data BoolExpression
  = LogicalAnd BoolExpression BoolExpression
  | LogicalOr BoolExpression BoolExpression
  | IdenticalNot Expression Expression
  | InequalitySlackLesser Expression Expression
  | InequalitySlackGreater Expression Expression
  | BoolTerminal BoolLiteral
  | LogicalNegate BoolExpression
  | AssociationQ String BoolExpression String
  deriving (Eq, Show)

data Class
  = Graph
  | Node
  deriving (Eq, Show)

type BoolLiteral = Bool
}