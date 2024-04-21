{
module Parser where

import Lexer
import Syntax
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  SELECT      { Tok _ TokSelect }
  FROM        { Tok _ TokFrom }
  WHERE       { Tok _ TokWhere }
  AND         { Tok _ TokAnd }
  OR          { Tok _ TokOr }
  LIMIT       { Tok _ TokLimit }
  CREATE      { Tok _ TokCreate }
  EDGE        { Tok _ TokEdge }
  TO          { Tok _ TokTo }
  TYPE        { Tok _ TokType }
  STARTS_WITH { Tok _ TokStartsWith }
  NOT         { Tok _ TokNot }
  UPDATE      { Tok _ TokUpdate }
  SET         { Tok _ TokSet }
  DELETE      { Tok _ TokDelete }
  ','         { Tok _ TokComma }
  '='         { Tok _ TokEqual }
  '<'         { Tok _ TokLT }
  '>'         { Tok _ TokGT }
  '<='        { Tok _ TokLTE }
  '>='        { Tok _ TokGTE }
  '('         { Tok _ TokLParen }
  ')'         { Tok _ TokRParen }
  '.'         { Tok _ TokPeriod }
  ':'         { Tok _ TokColon }
  '*'         { Tok _ TokAsterisk }
  '->'        { Tok _ TokArrow }
  '['         { Tok _ TokLBracket }
  ']'         { Tok _ TokRBracket }
  '-'         { Tok _ TokMinus }
  HAS         { Tok _ TokHas }
  IDENT       { Tok _ (TokIdent $$) }
  STRING      { Tok _ (TokString $$) }
  INT         { Tok _ (TokInt $$) }

%%

Query
  : SelectQuery                      { $1 }
  | CreateEdgeQuery                  { $1 }
  | UpdateQuery                      { $1 }
  | DeleteQuery                      { $1 }
  | DeleteEdgeQuery                  { $1 }

SelectQuery
  : SELECT PropertyRefs FROM NodePatterns WHERE Condition  { SelectQuery $2 $4 (Just $6) }
  | SELECT PropertyRefs FROM NodePatterns                  { SelectQuery $2 $4 Nothing }
  | SELECT '*' FROM NodePatterns WHERE Condition           { SelectQuery [PropertyRef "*" "*"] $4 (Just $6) }
  | SELECT '*' FROM NodePatterns                           { SelectQuery [PropertyRef "*" "*"] $4 Nothing }

CreateEdgeQuery
  : CREATE EDGE FROM NodePattern TO NodePattern TYPE EdgeType  { CreateEdgeQuery $4 $6 $8 }

UpdateQuery
  : UPDATE NodePatterns SET PropertyUpdates  { UpdateQuery $2 $4 }

DeleteQuery
  : DELETE NodePatterns WHERE Condition      { DeleteQuery $2 (Just $4) }
  | DELETE NodePatterns                      { DeleteQuery $2 Nothing }

DeleteEdgeQuery
  : DELETE EDGE TYPE EdgeType                { DeleteEdgeQuery $4 }

PropertyRefs
  : PropertyRef                       { [$1] }
  | PropertyRef ',' PropertyRefs      { $1 : $3 }

PropertyRef
  : IDENT '.' IDENT                   { PropertyRef $1 $3 }

NodePatterns
  : NodePattern                       { [$1] }
  | NodePattern ',' NodePatterns      { $1 : $3 }

NodePattern
  : '(' IDENT ')'                     { NodePattern $2 }

EdgeType
  : ':' IDENT                         { EdgeType $2 }

EdgePattern
    : '[' NodePattern '-' EdgeType '->' NodePattern ']'  { EdgePattern $2 $4 $6 }

Condition
  : Condition AND Condition           { ConditionAnd $1 $3 }
  | Condition OR Condition            { ConditionOr $1 $3 }
  | NOT Condition                     { ConditionNot $2 }
  | PropertyRef '=' Expression        { ConditionEqual $1 $3 }
  | PropertyRef '<' Expression        { ConditionLess $1 $3 }
  | PropertyRef '>' Expression        { ConditionGreater $1 $3 }
  | PropertyRef '<=' Expression       { ConditionLessEqual $1 $3 }
  | PropertyRef '>=' Expression       { ConditionGreaterEqual $1 $3 }
  | PropertyRef STARTS_WITH Expression { ConditionStartsWith $1 $3 }
  | PropertyRef HAS STRING            { ConditionHasLabel $1 $3 }
  | '(' NodePattern ')' '-' EdgePattern '->' '(' NodePattern ')'  { ConditionEdge $2 $5 $8 }
  | '(' Condition ')'                 { $2 }

PropertyUpdates
  : PropertyUpdate                    { [$1] }
  | PropertyUpdate ',' PropertyUpdates { $1 : $3 }

PropertyUpdate
  : PropertyRef '=' Expression        { PropertyUpdate $1 $3 }

Expression
  : INT                               { ExpressionInteger $1 }
  | STRING                            { ExpressionString $1 }
  | PropertyRef                       { ExpressionPropertyRef $1 }

{
parseError :: [Token] -> a
parseError tokens@(x:xs) = error $ "Parse error: " ++ tokenPosn (head tokens) ++ "\n" ++ show tokens
parseError [] = error "Parse error: end of input"
}