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
  NType                       { Tok _ TokNType          }
  RType                       { Tok _ TokRType          }
  True                        { Tok _ TokBT             }
  False                       { Tok _ TokBF             }
  var                         { Tok _ (TokIdent $$)     }
  int                         { Tok _ (TokInt $$)       }
  string                      { Tok _ (TokString $$)    }
  rgx                         { Tok _ (Tokrgx $$)       }
  '"'                         { Tok _ TokDelimiter      }
  '&&'                        { Tok _ TokConjunction    }
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
  '='                         { Tok _ TokAssign         }
  '=='                        { Tok _ TokEquals         }
  '['                         { Tok _ TokBracketLeftS   }
  ']'                         { Tok _ TokBracketRightS  }
  '->'                        { Tok _ TokDirectedR      }
  '-'                         { Tok _ TokHyph           }
  ':'                         { Tok _ TokNCol           }
  '{'                         { Tok _ TokLCurl          }
  '}'                         { Tok _ TokRCurl          }

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
  : Type var '=' Expr                       { TypedAssign $1 $2 $4 }
  | var '=' Expr                            { Assign $1 $3         }
  | Type var                                { Declare $1 $2        }
  | var                                     { Var $1               }
  | int                                     { Int $1               }
  | FIdent                                  { Var $1               }
  | string                                  { String $1            }
  | var '.' FIND '(' var '->' BoolExpr ')'  { FINDQuery $1 $5 $7   }
  | var '.' ADD '(' NewNode ')'             { AddQuery $1 $5       }
  | FILE string                             { FILE $2              }
  | OUT '(' var ')'                         { OUT $3               }
  | BoolExpr                                { BoolExpr $1          }
  | var '.' var                             { GetProperty $1 $3    }
  | var '.' FIdent                          { GetProperty $1 $3    }

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
  | '-' '[' BoolExpr ']' '->' var     { EndRelationQuery $3 $6   }
  | var '-' '[' BoolExpr ']' '->'     { StartRelationQuery $1 $4 }
  | '(' BoolExpr ')'                  { $2                       }
  | var '-' '[' BoolExpr ']' '->' var { RelationQuery $1 $4 $7   }
  | var '.' FIdent '==' string        { FIdentEquals $1 $3 $5    }

NewNode
  : var                               { NodeCopy $1 }
  | NodeAssignments                   { NewNode $1  }

NodeAssignments
  : NodeAssignment                      { [$1]      }
  | NodeAssignment ',' NodeAssignments  { ($1 : $3) }

NodeAssignment
  : var '=' Expr                              { NodeAssignment $1 $3        }
  | FIdent '=' Expr                           { NodeAssignment $1 $3        }
  | var '-' '[' NodeAssignments ']' '->' var  { RelationAssignment $1 $4 $7 }

IfStatement
  : IF '(' BoolExpr ')' '{' Program '}'                         { IfBlock $3 $6         }
  | IF '(' BoolExpr ')' '{' Program '}' ELSE '{' Program '}'    { IfElseBlock $3 $6 $10 }

ForStatement
  : FOR '(' Type var ':' var ')' '{' Program '}'         { ForBlock $3 $4 $6 $9 }

Type
  : GType         { Type $1 }
  | Num         { Type $1 }
  | Chars         { Type $1 }
  | B         { Type $1 }
  | NType         { Type $1 }
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
  | AddQuery String Node
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
  | EndRelationQuery BoolExpr String
  | StartRelationQuery String BoolExpr
  | RelationQuery String BoolExpr String
  | FIdentEquals String String String
  deriving(Eq, Show)
  
data Node
  = NodeCopy String
  | NewNode [NodeAssignment]
  deriving(Eq, Show)

data NodeAssignment
  = NodeAssignment String Expr
  | RelationAssignment String [NodeAssignment] String
  deriving(Eq, Show)

data Type
  = Type Token
  deriving(Eq, Show)

} 