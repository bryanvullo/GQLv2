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
  CONDITIONELIF                        { Tok _ TokCondE          }
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
  : Y ';'                        { Y $1 }
  | ConditionX                     { $1      }
  | LoopFX                    { $1      }

Y
  : Class ident '=' Y                       { IdentCl $1 $2 $4 }
  | ident '=' Y                            { Ident $1 $3         }
  | Class ident                                { IdentFin $1 $2        }
  | ident                                     { IdentT $1               }
  | num                                     { Int $1               }
  | FIdent                                  { IdentT $1               }
  | chars                                  { String $1            }
  | ident '.' FIND '(' ident '->' YBool ')'  { FINDCall $1 $5 $7   }
  | ident '.' ARITH '(' AddGrN ')'             { ArithCall $1 $5       }
  | ACCESS chars                             { ACCESS $2              }
  | OUT '(' ident ')'                         { OUT $3               }
  | YBool                                { YBool $1          }
  | ident '.' ident                             { IdentChar $1 $3    }
  | ident '.' FIdent                          { IdentChar $1 $3    }

YBool
  : True                              { Bool True                }
  | False                             { Bool False               }
  | Y '==' Y                    { Exact $1 $3             }
  | Y '!=' Y                    { Ineq $1 $3          }
  | Y '<' Y                     { EqualityL $1 $3           }
  | Y '>' Y                     { EqualityG $1 $3        }
  | Y '<=' Y                    { EqualityEqL $1 $3           }
  | Y '>=' Y                    { EqualityEqG $1 $3           }
  | YBool '&&' YBool            { And $1 $3                }
  | YBool '||' YBool            { LogO $1 $3                 }
  | '-' '[' YBool ']' '->' ident     { RelCallFin $3 $6   }
  | ident '-' '[' YBool ']' '->'     { RelCallNew $1 $4 }
  | '(' YBool ')'                  { $2                       }
  | ident '-' '[' YBool ']' '->' ident { RelCall $1 $4 $7   }
  | ident '.' FIdent '==' chars        { FIdentExact $1 $3 $5    }

AddGrN
  : ident                               { GrNDup $1 }
  | SetGrNNT                   { AddGrN $1  }

SetGrNNT
  : SetGrNT                      { [$1]      }
  | SetGrNT ',' SetGrNNT  { ($1 : $3) }

SetGrNT
  : ident '=' Y                              { SetGrNT $1 $3        }
  | FIdent '=' Y                           { SetGrNT $1 $3        }
  | ident '-' '[' SetGrNNT ']' '->' ident  { RelSet $1 $4 $7 }

ConditionX
  : CONDITION '(' YBool ')' '{' XX '}'                         { ConditionBXX $3 $6         }
  | CONDITION '(' YBool ')' '{' XX '}' CONDITIONELIF '{' XX '}'    { ConditionBXXEXX $3 $6 $10 }

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
  = Y Y
  | ConditionBXX YBool XX
  | ConditionBXXEXX YBool XX XX
  | LoopFBlock Class String String XX
  deriving(Eq, Show)

data Y
  = IdentCl Class String Y
  | Ident String Y
  | IdentFin Class String
  | IdentT String
  | Int Int
  | String String
  | FINDCall String String YBool
  | ArithCall String GrN
  | ACCESS String
  | OUT String
  | YBool YBool
  | IdentChar String String
  deriving(Eq, Show)

data YBool
  = Bool Bool
  | Exact Y Y
  | Ineq Y Y
  | EqualityL Y Y
  | EqualityG Y Y
  | EqualityEqL Y Y
  | EqualityEqG Y Y
  | And YBool YBool
  | LogO YBool YBool
  | RelCallFin YBool String
  | RelCallNew String YBool
  | RelCall String YBool String
  | FIdentExact String String String
  deriving(Eq, Show)
  
data GrN
  = GrNDup String
  | AddGrN [SetGrNT]
  deriving(Eq, Show)

data SetGrNT
  = SetGrNT String Y
  | RelSet String [SetGrNT] String
  deriving(Eq, Show)

data Class
  = Class Token
  deriving(Eq, Show)

} 