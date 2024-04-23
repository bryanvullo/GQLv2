{
module Parser (parser, Program) where
import Lexer
}

%name parser
%tokentype { Token }

%error { parseError }

%token
  FILE                        { Tok _ TokFILE           }
  FIND                        { Tok _ TokFIND           }
  OUT                         { Tok _ TokOUT            }
  IF                          { Tok _ TokCond           }
  ELSE                        { Tok _ TokElse           }
  FOR                         { Tok _ TokFor            }
  ADD                         { Tok _ TokArith          }
  GType                       { Tok _ TokGr             }
  Num                         { Tok _ TokNum          }
  Chars                       { Tok _ TokChars          }
  B                           { Tok _ TokB          }
  FIdent                      { Tok _ (TokFIdent $$)    }
  GrN                         { Tok _ TokGrN          }
  RType                       { Tok _ TokRType          }
  True                        { Tok _ TokBT             }
  False                       { Tok _ TokBF             }
  ident                         { Tok _ (TokIdent $$)     }
  int                         { Tok _ (TokInt $$)       }
  string                      { Tok _ (TokString $$)    }
  rgx                         { Tok _ (Tokrgx $$)       }
  '"'                         { Tok _ TokDelimiter      }
  '&&'                        { Tok _ TokConj    }
  '||'                        { Tok _ TokOr             }
  '('                         { Tok _ TokBracketLeft    }
  ')'                         { Tok _ TokBracketRight   }
  ';'                         { Tok _ TokSCol           }
  '!='                        { Tok _ TokIneq           }
  ','                         { Tok _ TokSep            }
  '.'                         { Tok _ TokBrk            }
  '>='                        { Tok _ TokEqualityG      }
  '<='                        { Tok _ TokEqualityL      }
  '>'                         { Tok _ TokGT             }
  '<'                         { Tok _ TokLT             }
  '='                         { Tok _ TokSet         }
  '=='                        { Tok _ TokEquals         }
  '['                         { Tok _ TokBracketLeftS   }
  ']'                         { Tok _ TokBracketRightS  }
  '->'                        { Tok _ TokDirectedR      }
  '-'                         { Tok _ TokHyph           }
  ':'                         { Tok _ TokNCol           }
  '{'                         { Tok _ TokBracketLeftC          }
  '}'                         { Tok _ TokBracketRightC          }

%right '||'
%right '&&'

%%

Program
  : Statement                       { [$1]      }
  | Statement Program               { ($1 : $2) }

Statement
  : Expr ';'                        { Expr $1 }
  | IfStatement                     { $1      }
  | ForStatement                    { $1      }

Expr
  : Type ident '=' Expr                       { TypedAssign $1 $2 $4 }
  | ident '=' Expr                            { Assign $1 $3         }
  | Type ident                                { Declare $1 $2        }
  | ident                                     { Var $1               }
  | int                                     { Int $1               }
  | FIdent                                  { Var $1               }
  | string                                  { String $1            }
  | ident '.' FIND '(' ident '->' BoolExpr ')'  { FINDQuery $1 $5 $7   }
  | ident '.' ADD '(' NewGraphNode ')'             { AddQuery $1 $5       }
  | FILE string                             { FILE $2              }
  | OUT '(' ident ')'                         { OUT $3               }
  | BoolExpr                                { BoolExpr $1          }
  | ident '.' ident                             { GetProperty $1 $3    }
  | ident '.' FIdent                          { GetProperty $1 $3    }

BoolExpr
  : True                              { Bool True                }
  | False                             { Bool False               }
  | Expr '==' Expr                    { Equals $1 $3             }
  | Expr '!=' Expr                    { Ineq $1 $3          }
  | Expr '<' Expr                     { LessThan $1 $3           }
  | Expr '>' Expr                     { GreaterThan $1 $3        }
  | Expr '<=' Expr                    { LTEquals $1 $3           }
  | Expr '>=' Expr                    { GTEquals $1 $3           }
  | BoolExpr '&&' BoolExpr            { And $1 $3                }
  | BoolExpr '||' BoolExpr            { Or $1 $3                 }
  | '-' '[' BoolExpr ']' '->' ident     { EndRelQuery $3 $6   }
  | ident '-' '[' BoolExpr ']' '->'     { StartRelQuery $1 $4 }
  | '(' BoolExpr ')'                  { $2                       }
  | ident '-' '[' BoolExpr ']' '->' ident { RelQuery $1 $4 $7   }
  | ident '.' FIdent '==' string        { FIdentEquals $1 $3 $5    }

NewGraphNode
  : ident                               { GraphNodeCopy $1 }
  | GraphNodeSetNT                   { NewGraphNode $1  }

GraphNodeSetNT
  : GraphNodeSet                      { [$1]      }
  | GraphNodeSet ',' GraphNodeSetNT  { ($1 : $3) }

GraphNodeSet
  : ident '=' Expr                              { GraphNodeSet $1 $3        }
  | FIdent '=' Expr                           { GraphNodeSet $1 $3        }
  | ident '-' '[' GraphNodeSetNT ']' '->' ident  { RelSet $1 $4 $7 }

IfStatement
  : IF '(' BoolExpr ')' '{' Program '}'                         { IfBlock $3 $6         }
  | IF '(' BoolExpr ')' '{' Program '}' ELSE '{' Program '}'    { IfElseBlock $3 $6 $10 }

ForStatement
  : FOR '(' Type ident ':' ident ')' '{' Program '}'         { ForBlock $3 $4 $6 $9 }

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
  = Expr Expr
  | IfBlock BoolExpr Program
  | IfElseBlock BoolExpr Program Program
  | ForBlock Type String String Program
  deriving(Eq, Show)

data Expr
  = TypedAssign Type String Expr
  | Assign String Expr
  | Declare Type String
  | Var String
  | Int Int
  | String String
  | FINDQuery String String BoolExpr
  | AddQuery String GraphNode
  | FILE String
  | OUT String
  | BoolExpr BoolExpr
  | GetProperty String String
  deriving(Eq, Show)

data BoolExpr
  = Bool Bool
  | Equals Expr Expr
  | Ineq Expr Expr
  | LessThan Expr Expr
  | GreaterThan Expr Expr
  | LTEquals Expr Expr
  | GTEquals Expr Expr
  | And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  | EndRelQuery BoolExpr String
  | StartRelQuery String BoolExpr
  | RelQuery String BoolExpr String
  | FIdentEquals String String String
  deriving(Eq, Show)
  
data GraphNode
  = GraphNodeCopy String
  | NewGraphNode [GraphNodeSet]
  deriving(Eq, Show)

data GraphNodeSet
  = GraphNodeSet String Expr
  | RelSet String [GraphNodeSet] String
  deriving(Eq, Show)

data Type
  = Type Token
  deriving(Eq, Show)

} 