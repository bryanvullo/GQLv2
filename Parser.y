{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token
  n                     { Key (KeyNum $$)         _ }
  ACCESS                { Key KeyACCESSToken      _ }
  CASE                  { Key KeyCASEToken        _ }
  STDOUT                { Key KeySTDOUTToken      _ }
  AND                   { Key KeyLogicalAnd       _ }
  OR                    { Key KeyLogicalOr        _ }
  '('                   { Key KeyBracketLeft      _ }
  ')'                   { Key KeyBracketRight     _ }
  Graph                 { Key KeyGraphToken       _ }
  HAS                   { Key KeyHasToken         _ }
  CONDIF                { Key KeyCONDIFToken      _ }
  THROUGH               { Key KeyTHROUGHToken     _ }
  identity              { Key (KeyIdentity $$)    _ }
  '>>'                  { Key KeyGreaterEqual     _ }
  '<<'                  { Key KeyLessEqual        _ }
  '>'                   { Key KeyGreater          _ }
  '<'                   { Key KeyLess             _ }
  '='                   { Key KeyAssign           _ }
  'i=='                 { Key KeyEqual            _ }
  '!=='                 { Key KeyNotEqual         _ }
  PLUS                  { Key KeyPlusToken        _ }
  '.'                   { Key KeyDot              _ }
  chars                 { Key (KeyChars $$)       _ }
  True                  { Key KeyTrue             _ }
  False                 { Key KeyFalse            _ }
  '-'                   { Key KeyMinus            _ }
  DataPoint             { Key KeyDataPointToken   _ }
  Association           { Key KeyAssociationToken _ }
  CALLASSOCIATION       { Key KeyCallAssocToken   _ }
  CALLDATAPOINT         { Key KeyCallDataToken    _ }
  '^'                   { Key KeyEdge             _ }
  '{'                   { Key KeyBraceLeft        _ }
  '}'                   { Key KeyBraceRight       _ }
  ':'                   { Key KeyColon            _ }

%right '='
%left OR AND
%nonassoc '>' '<' '>>' '<<' 'i==' '!==' 
%left '+' '-'

%%

Program : Statement Program  { $1 : $2 }
        | {- empty -}        { []     }

Statement : Expr              { Expr $1    }
          | Condif            { CondifStmt $1  }
          | Through           { ThroughStmt $1 }
          | Assign            { AssignStmt $1  }

Expr : DataStruct            { $1 }
     | Access                { $1 }
     | Stdout                { $1 }
     | Identifier            { $1 }
     | Plus                  { $1 }
     | '(' Expr ')'          { $2 }
     | Case                  { $1 }

DataStruct : Graph identity '=' Access    { DataStructSet $2 $4 }
           | Graph identity               { DataStructShow $2   }

Access : ACCESS '(' chars ')'             { Access $3 }

Stdout : STDOUT '(' identity ')'          { Stdout $3 }

Identifier : identity                     { Identifier $1 }
           | identity '.' identity        { HeaderAttribute $1 $3}

Case : identity '.' CASE '(' BoolExpr ')'     { Case $1 $5   }

Plus : Identifier '.' PLUS '(' Expr ')'         { Plus $1 $5   }

BoolExpr : BoolTerm AND BoolExpr           { And $1 $3       }
         | BoolTerm OR BoolExpr            { Or $1 $3        }
         | BoolTerm                        { $1              }

BoolTerm : Expr '.' HAS '(' chars ')'            { Has $1 $5          }
         | Expr 'i==' Expr                       { Equal $1 $3        }  
         | Expr '!==' Expr                       { NotEqual $1 $3     }
         | Expr '>>' Expr                        { GreaterEqual $1 $3 }
         | Expr '<<' Expr                        { LessEqual $1 $3    }
         | Expr '>' Expr                         { Greater $1 $3      }  
         | Expr '<' Expr                         { Less $1 $3         }
         | True                                  { BoolLit True       }
         | False                                 { BoolLit False      }
         | identity '-' Association '^' Expr     { EdgeBoolTerm $1 $5 }
         | '(' BoolExpr ')'                      { $2                 }

Condif : CONDIF '(' BoolExpr ')' '{' Statements '}'      { Condif $3 $6 }

Through : THROUGH '(' DataPoint identity ':' Expr ')' '{' Statements '}'  { Through DataPoint $4 $6 $9 }

Assign : identity '=' Expr                              { Assign $1 $3 }

Statements : Statement Statements     { $1 : $2 }
           | {- empty -}              { []      }

{
parseError :: [Token] -> a
parseError [] = error "Parse error at end of input"
parseError (t:_) = error $ "Parse error at Ln " ++ show (getLn (getPos t)) ++ " Col " ++ show (getCol (getPos t)) ++ " token: " ++ show t
  where
    getPos (Key _ p) = p
    getLn (AlexPn _ l _) = l
    getCol (AlexPn _ _ c) = c

data Statement
  = Expr Expr
  | CondifStmt Condif  
  | ThroughStmt Through
  | AssignStmt Assign
  deriving (Eq, Show)

data Expr
  = DataStructSet String Expr
  | DataStructShow String
  | Access String
  | Stdout String
  | Identifier String
  | Attribute String String
  | HeaderAttribute String String
  | Case String BoolExpr
  | Plus Expr Expr
  | Edge String Association Expr
  deriving (Eq, Show)

data BoolExpr  
  = And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  | Has Expr String
  | Equal Expr Expr
  | NotEqual Expr Expr
  | GreaterEqual Expr Expr
  | LessEqual Expr Expr 
  | Greater Expr Expr
  | Less Expr Expr
  | BoolLit Bool
  | EdgeBoolTerm String Expr
  deriving (Eq, Show)

data Condif = Condif BoolExpr [Statement]
  deriving (Eq, Show)

data Through = Through DataPoint String Expr [Statement]  
  deriving (Eq, Show)

data Assign = Assign String Expr
  deriving (Eq, Show)

data DataPoint = DataPoint
  deriving (Eq, Show)

data Association = Association  
  deriving (Eq, Show)
}