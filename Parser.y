{
module Parser (parser, Program) where
import Lexer
}

%name parser
%tokentype { Token }

%error { parseError }

%token
  ACCESS                        { Tok _ TokACCESS           }
  FIND                        { Tok _ TokFIND           }
  OUT                         { Tok _ TokOUT            }
  CONDITION                          { Tok _ TokCond           }
  ELSE                        { Tok _ TokCondE          }
  LOOPF                         { Tok _ TokLoopF          }
  ARITH                         { Tok _ TokArith          }
  GType                       { Tok _ TokGr             }
  Num                         { Tok _ TokNum            }
  Chars                       { Tok _ TokChars          }
  B                           { Tok _ TokB              }
  FIdent                      { Tok _ (TokFIdent $$)    }
  GrN                         { Tok _ TokGrN            }
  RType                       { Tok _ TokRel          }
  True                        { Tok _ TokBT             }
  False                       { Tok _ TokBF             }
  ident                       { Tok _ (TokIdent $$)     }
  num                         { Tok _ (TokInt $$)       }
  string                      { Tok _ (TokString $$)    }
  Reg                         { Tok _ (TokReg $$)       }
  '"'                         { Tok _ TokQuote      }
  '&&'                        { Tok _ TokConj           }
  '||'                        { Tok _ TokLogO             }
  '('                         { Tok _ TokBracketLeft    }
  ')'                         { Tok _ TokBracketRight   }
  ';'                         { Tok _ TokColS           }
  '!='                        { Tok _ TokIneq           }
  ','                         { Tok _ TokSep            }
  '.'                         { Tok _ TokBrk            }
  '>='                        { Tok _ TokEqualityEqG      }
  '<='                        { Tok _ TokEqualityEqL      }
  '>'                         { Tok _ TokEqualityG             }
  '<'                         { Tok _ TokEqualityL             }
  '='                         { Tok _ TokSet            }
  '=='                        { Tok _ TokExact         }
  '['                         { Tok _ TokBracketLeftS   }
  ']'                         { Tok _ TokBracketRightS  }
  '->'                        { Tok _ TokDirectedR      }
  '-'                         { Tok _ TokHyph           }
  ':'                         { Tok _ TokHeaderColN           }
  '{'                         { Tok _ TokBracketLeftC   }
  '}'                         { Tok _ TokBracketRightC  }

%right '||'
%right '&&'

%%

Program
  : Statement                       { [$1]      }
  | Statement Program               { ($1 : $2) }

Statement
  : E ';'                        { E $1 }
  | ConditionStatement                     { $1      }
  | LoopFStatement                    { $1      }

E
  : Type ident '=' E                       { TypedAssign $1 $2 $4 }
  | ident '=' E                            { Assign $1 $3         }
  | Type ident                                { Declare $1 $2        }
  | ident                                     { Var $1               }
  | num                                     { Int $1               }
  | FIdent                                  { Var $1               }
  | string                                  { String $1            }
  | ident '.' FIND '(' ident '->' BoolE ')'  { FINDCall $1 $5 $7   }
  | ident '.' ARITH '(' NewGrNode ')'             { ArithCall $1 $5       }
  | ACCESS string                             { ACCESS $2              }
  | OUT '(' ident ')'                         { OUT $3               }
  | BoolE                                { BoolE $1          }
  | ident '.' ident                             { GetProperty $1 $3    }
  | ident '.' FIdent                          { GetProperty $1 $3    }

BoolE
  : True                              { Bool True                }
  | False                             { Bool False               }
  | E '==' E                    { Equals $1 $3             }
  | E '!=' E                    { Ineq $1 $3          }
  | E '<' E                     { LessThan $1 $3           }
  | E '>' E                     { GreaterThan $1 $3        }
  | E '<=' E                    { LTEquals $1 $3           }
  | E '>=' E                    { GTEquals $1 $3           }
  | BoolE '&&' BoolE            { And $1 $3                }
  | BoolE '||' BoolE            { Or $1 $3                 }
  | '-' '[' BoolE ']' '->' ident     { EndRelCall $3 $6   }
  | ident '-' '[' BoolE ']' '->'     { StartRelCall $1 $4 }
  | '(' BoolE ')'                  { $2                       }
  | ident '-' '[' BoolE ']' '->' ident { RelCall $1 $4 $7   }
  | ident '.' FIdent '==' string        { FIdentEquals $1 $3 $5    }

NewGrNode
  : ident                               { GrNodeCopy $1 }
  | GrNodeSetNT                   { NewGrNode $1  }

GrNodeSetNT
  : GrNodeSet                      { [$1]      }
  | GrNodeSet ',' GrNodeSetNT  { ($1 : $3) }

GrNodeSet
  : ident '=' E                              { GrNodeSet $1 $3        }
  | FIdent '=' E                           { GrNodeSet $1 $3        }
  | ident '-' '[' GrNodeSetNT ']' '->' ident  { RelSet $1 $4 $7 }

ConditionStatement
  : CONDITION '(' BoolE ')' '{' Program '}'                         { ConditionBlock $3 $6         }
  | CONDITION '(' BoolE ')' '{' Program '}' ELSE '{' Program '}'    { ConditionElseBlock $3 $6 $10 }

LoopFStatement
  : LOOPF '(' Type ident ':' ident ')' '{' Program '}'         { LoopFBlock $3 $4 $6 $9 }

Type
  : GType         { Type $1 }
  | Num         { Type $1 }
  | Chars         { Type $1 }
  | B         { Type $1 }
  | GrN         { Type $1 }
  | RType         { Type $1 }

{
parseError :: [Token] -> a
parseError [] = error "Should not be erroring on no Tokens"
parseError (Tok (AlexPn _ r c) t : _) = error $ "Parse error on token: " ++ show t ++ ", at: " ++ show r ++ ":" ++ show c ++ "\n"

type Program
  = [Statement]

data Statement
  = E E
  | ConditionBlock BoolE Program
  | ConditionElseBlock BoolE Program Program
  | LoopFBlock Type String String Program
  deriving(Eq, Show)

data E
  = TypedAssign Type String E
  | Assign String E
  | Declare Type String
  | Var String
  | Int Int
  | String String
  | FINDCall String String BoolE
  | ArithCall String GrNode
  | ACCESS String
  | OUT String
  | BoolE BoolE
  | GetProperty String String
  deriving(Eq, Show)

data BoolE
  = Bool Bool
  | Equals E E
  | Ineq E E
  | LessThan E E
  | GreaterThan E E
  | LTEquals E E
  | GTEquals E E
  | And BoolE BoolE
  | Or BoolE BoolE
  | EndRelCall BoolE String
  | StartRelCall String BoolE
  | RelCall String BoolE String
  | FIdentEquals String String String
  deriving(Eq, Show)
  
data GrNode
  = GrNodeCopy String
  | NewGrNode [GrNodeSet]
  deriving(Eq, Show)

data GrNodeSet
  = GrNodeSet String E
  | RelSet String [GrNodeSet] String
  deriving(Eq, Show)

data Type
  = Type Token
  deriving(Eq, Show)

} 