{
module Parser (parser, Program) where
import Lexer
}

%name parser
%tokentype { Token }

%error { parseError }

%token
  int                         { Tok _ (TokInt $$)       }
  FILE                        { Tok _ TokFILE           }
  FIND                        { Tok _ TokFIND           }
  OUT                         { Tok _ TokOUT            }
  '"'                         { Tok _ TokDelimiter      }
  '&&'                        { Tok _ TokAnd            }
  '||'                        { Tok _ TokOr             }
  '('                         { Tok _ TokLParen         }
  ')'                         { Tok _ TokRParen         }
  GType                       { Tok _ TokGType          }
  IType                       { Tok _ TokIType          }
  SType                       { Tok _ TokSType          }
  BType                       { Tok _ TokBType          }
  '{'                         { Tok _ TokLCurl          }
  '}'                         { Tok _ TokRCurl          }
  IF                          { Tok _ TokIf             }
  ELSE                        { Tok _ TokElse           }
  FOR                         { Tok _ TokFor            }
  ':'                         { Tok _ TokColon          }
  var                         { Tok _ (TokIdent $$)     }
  '>='                        { Tok _ TokGEQ            }
  '<='                        { Tok _ TokLEQ            }
  '>'                         { Tok _ TokGT             }
  '<'                         { Tok _ TokLT             }
  '='                         { Tok _ TokAssign         }
  '=='                        { Tok _ TokEquals         }
  '['                         { Tok _ TokLSQ            }
  ']'                         { Tok _ TokRSQ            }
  '->'                        { Tok _ TokRArrow         }
  '-'                         { Tok _ TokDash           }
  regex                       { Tok _ (TokRegex $$)     }
  ADD                         { Tok _ TokAdd            }
  '.'                         { Tok _ TokDot            }
  bigField                    { Tok _ (TokBigField $$)  }
  string                      { Tok _ (TokString $$)    }
  True                        { Tok _ TokTrue           }
  False                       { Tok _ TokFalse          }
  ';'                         { Tok _ TokSemicolon      }
  '!='                        { Tok _ TokNotEquals      }
  ','                         { Tok _ TokComma          }
  NType                    { Tok _ TokNType       }
  RType                { Tok _ TokRType   }

%right '||'
%right '&&'

%%

Program
  : Statement                       { [$1] }
  | Statement Program               { ($1 : $2) }

Statement
  : Expr ';'                        { Expr $1 }
  | IfStatement                     { $1 }
  | ForStatement                    { $1 }

Expr
  : Type var '=' Expr               { TypedAssign $1 $2 $4 }
  | var '=' Expr                    { Assign $1 $3 }
  | Type var                        { Declare $1 $2 }
  | var                             { Var $1 }
  | int                             { Int $1 }
  | bigField                        { Var $1 }
  | string                          { String $1 }
  | var '.' FIND '(' var '->' BoolExpr ')'  { FINDQuery $1 $5 $7 }
  | var '.' ADD '(' NewNode ')'     { AddQuery $1 $5 }
  | FILE string                 { FILE $2 }
  | OUT '(' var ')'               { OUT $3 }
  | BoolExpr                        { BoolExpr $1 }
  | var '.' var                     { GetProperty $1 $3 }
  | var '.' bigField                { GetProperty $1 $3 }

BoolExpr
  : True                              { Bool True }
  | False                             { Bool False }
  | Expr '==' Expr                    { Equals $1 $3 }
  | Expr '!=' Expr                    { NotEquals $1 $3}
  | Expr '<' Expr                     { LessThan $1 $3 }
  | Expr '>' Expr                     { GreaterThan $1 $3 }
  | Expr '<=' Expr                    { LTEquals $1 $3 }
  | Expr '>=' Expr                    { GTEquals $1 $3 }
  | BoolExpr '&&' BoolExpr            { And $1 $3 }
  | BoolExpr '||' BoolExpr            { Or $1 $3 }
  | '-' '[' BoolExpr ']' '->' var     { EndRelationQuery $3 $6 }
  | var '-' '[' BoolExpr ']' '->'     { StartRelationQuery $1 $4 }
  | '(' BoolExpr ')'                  { $2 }
  | var '-' '[' BoolExpr ']' '->' var { RelationQuery $1 $4 $7}
  | var '.' bigField '==' string      { BigFieldEquals $1 $3 $5 }

NewNode
  : var                               { NodeCopy $1 }
  | NodeAssignments                   { NewNode $1 }

NodeAssignments
  : NodeAssignment                      { [$1] }
  | NodeAssignment ',' NodeAssignments  { ($1 : $3) }

NodeAssignment
  : var '=' Expr                              { NodeAssignment $1 $3 }
  | bigField '=' Expr                         { NodeAssignment $1 $3 }
  | var '-' '[' NodeAssignments ']' '->' var  { RelationAssignment $1 $4 $7 }

IfStatement
  : IF '(' BoolExpr ')' '{' Program '}'                         { IfBlock $3 $6 }
  | IF '(' BoolExpr ')' '{' Program '}' ELSE '{' Program '}'    { IfElseBlock $3 $6 $10 }

ForStatement
  : FOR '(' Type var ':' var ')' '{' Program '}'         { ForBlock $3 $4 $6 $9 }

Type
  : GType         { Type $1 }
  | IType       { Type $1 }
  | SType        { Type $1 }
  | BType       { Type $1 }
  | NType          { Type $1 }
  | RType      { Type $1 }

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
  | NotEquals Expr Expr
  | LessThan Expr Expr
  | GreaterThan Expr Expr
  | LTEquals Expr Expr
  | GTEquals Expr Expr
  | And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  | EndRelationQuery BoolExpr String
  | StartRelationQuery String BoolExpr
  | RelationQuery String BoolExpr String
  | BigFieldEquals String String String
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