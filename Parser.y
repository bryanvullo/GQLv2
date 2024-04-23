{
module Parser (parser, XX) where
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
  Gr                       { Tok _ TokGr             }
  Num                         { Tok _ TokNum            }
  Chars                       { Tok _ TokChars          }
  B                           { Tok _ TokB              }
  FIdent                      { Tok _ (TokFIdent $$)    }
  GrN                         { Tok _ TokGrN            }
  Rel                       { Tok _ TokRel          }
  True                        { Tok _ TokBT             }
  False                       { Tok _ TokBF             }
  ident                       { Tok _ (TokIdent $$)     }
  num                         { Tok _ (TokInt $$)       }
  chars                      { Tok _ (TokString $$)    }
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

XX
  : X                       { [$1]      }
  | X XX               { ($1 : $2) }

X
  : E ';'                        { E $1 }
  | ConditionX                     { $1      }
  | LoopFX                    { $1      }

E
  : Class ident '=' E                       { IdentCl $1 $2 $4 }
  | ident '=' E                            { Ident $1 $3         }
  | Class ident                                { IdentFin $1 $2        }
  | ident                                     { Var $1               }
  | num                                     { Int $1               }
  | FIdent                                  { Var $1               }
  | chars                                  { String $1            }
  | ident '.' FIND '(' ident '->' BoolE ')'  { FINDCall $1 $5 $7   }
  | ident '.' ARITH '(' NewGrNode ')'             { ArithCall $1 $5       }
  | ACCESS chars                             { ACCESS $2              }
  | OUT '(' ident ')'                         { OUT $3               }
  | BoolE                                { BoolE $1          }
  | ident '.' ident                             { IdentChar $1 $3    }
  | ident '.' FIdent                          { IdentChar $1 $3    }

BoolE
  : True                              { Bool True                }
  | False                             { Bool False               }
  | E '==' E                    { Exact $1 $3             }
  | E '!=' E                    { Ineq $1 $3          }
  | E '<' E                     { LessThan $1 $3           }
  | E '>' E                     { GreaterThan $1 $3        }
  | E '<=' E                    { LTExact $1 $3           }
  | E '>=' E                    { GTExact $1 $3           }
  | BoolE '&&' BoolE            { And $1 $3                }
  | BoolE '||' BoolE            { Or $1 $3                 }
  | '-' '[' BoolE ']' '->' ident     { RelCallFin $3 $6   }
  | ident '-' '[' BoolE ']' '->'     { RelCallNew $1 $4 }
  | '(' BoolE ')'                  { $2                       }
  | ident '-' '[' BoolE ']' '->' ident { RelCall $1 $4 $7   }
  | ident '.' FIdent '==' chars        { FIdentExact $1 $3 $5    }

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

ConditionX
  : CONDITION '(' BoolE ')' '{' XX '}'                         { ConditionBXX $3 $6         }
  | CONDITION '(' BoolE ')' '{' XX '}' ELSE '{' XX '}'    { ConditionBXXEXX $3 $6 $10 }

LoopFX
  : LOOPF '(' Class ident ':' ident ')' '{' XX '}'         { LoopFBlock $3 $4 $6 $9 }

Class
  : Gr         { Class $1 }
  | Num         { Class $1 }
  | Chars         { Class $1 }
  | B         { Class $1 }
  | GrN         { Class $1 }
  | Rel         { Class $1 }

{
parseError :: [Token] -> a
parseError [] = error "Parse error at end of file\n"
parseError (Tok (AlexPn _ r c) t : _) = error $ "Error " ++ show t ++ " @ " ++ show r ++ " - " ++ show c ++ "\n"

type XX
  = [X]

data X
  = E E
  | ConditionBXX BoolE XX
  | ConditionBXXEXX BoolE XX XX
  | LoopFBlock Class String String XX
  deriving(Eq, Show)

data E
  = IdentCl Class String E
  | Ident String E
  | IdentFin Class String
  | Var String
  | Int Int
  | String String
  | FINDCall String String BoolE
  | ArithCall String GrNode
  | ACCESS String
  | OUT String
  | BoolE BoolE
  | IdentChar String String
  deriving(Eq, Show)

data BoolE
  = Bool Bool
  | Exact E E
  | Ineq E E
  | LessThan E E
  | GreaterThan E E
  | LTExact E E
  | GTExact E E
  | And BoolE BoolE
  | Or BoolE BoolE
  | RelCallFin BoolE String
  | RelCallNew String BoolE
  | RelCall String BoolE String
  | FIdentExact String String String
  deriving(Eq, Show)
  
data GrNode
  = GrNodeCopy String
  | NewGrNode [GrNodeSet]
  deriving(Eq, Show)

data GrNodeSet
  = GrNodeSet String E
  | RelSet String [GrNodeSet] String
  deriving(Eq, Show)

data Class
  = Class Token
  deriving(Eq, Show)

} 