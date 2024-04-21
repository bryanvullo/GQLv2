{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$lower = [a-z]
$upper = [A-Z]
$alpha = [$lower $upper]
$alphanum = [$alpha $digit]

tokens :-

  $white+                           ;
  "--".*                            ;

  "SELECT"                          { \p _ -> Tok p TokSelect }
  "FROM"                            { \p _ -> Tok p TokFrom }
  "WHERE"                           { \p _ -> Tok p TokWhere }
  "AND"                             { \p _ -> Tok p TokAnd }
  "OR"                              { \p _ -> Tok p TokOr }
  "LIMIT"                           { \p _ -> Tok p TokLimit }
  "CREATE"                          { \p _ -> Tok p TokCreate }
  "EDGE"                            { \p _ -> Tok p TokEdge }
  "TO"                              { \p _ -> Tok p TokTo }
  "TYPE"                            { \p _ -> Tok p TokType }
  "STARTS WITH"                     { \p _ -> Tok p TokStartsWith }
  "NOT"                             { \p _ -> Tok p TokNot }
  "UPDATE"                          { \p _ -> Tok p TokUpdate }
  "SET"                             { \p _ -> Tok p TokSet }
  "DELETE"                          { \p _ -> Tok p TokDelete }
  ","                               { \p _ -> Tok p TokComma }
  "="                               { \p _ -> Tok p TokEqual }
  "<"                               { \p _ -> Tok p TokLT }
  ">"                               { \p _ -> Tok p TokGT }
  "<="                              { \p _ -> Tok p TokLTE }
  ">="                              { \p _ -> Tok p TokGTE }
  "("                               { \p _ -> Tok p TokLParen }
  ")"                               { \p _ -> Tok p TokRParen }
  "."                               { \p _ -> Tok p TokPeriod }
  ":"                               { \p _ -> Tok p TokColon }
  "*"                               { \p _ -> Tok p TokAsterisk }
  "->"                              { \p _ -> Tok p TokArrow }
  "["                               { \p _ -> Tok p TokLBracket }
  "]"                               { \p _ -> Tok p TokRBracket }
  "-"                               { \p _ -> Tok p TokMinus }
  "HAS"                             { \p _ -> Tok p TokHas }

  $digit+                           { \p s -> Tok p (TokInt (read s :: Integer)) }
  \" ([^\\\"]|\\.)*\"               { \p s -> Tok p (TokString s) }
  $alpha [$alpha $digit \_ \']*     { \p s -> Tok p (TokIdent s) }

{
data TokenType
  = TokSelect
  | TokFrom
  | TokWhere
  | TokAnd
  | TokOr
  | TokLimit
  | TokCreate
  | TokEdge
  | TokTo
  | TokType
  | TokStartsWith
  | TokNot
  | TokUpdate
  | TokSet
  | TokDelete
  | TokComma
  | TokEqual
  | TokLT
  | TokGT
  | TokLTE
  | TokGTE
  | TokLParen
  | TokRParen
  | TokPeriod
  | TokColon
  | TokAsterisk
  | TokArrow
  | TokLBracket
  | TokRBracket
  | TokMinus
  | TokHas
  | TokIdent String
  | TokString String
  | TokInt Integer
  deriving (Eq, Show)

data Token = Tok AlexPosn TokenType
  deriving (Eq, Show)

tokenPosn :: Token -> String
tokenPosn (Tok (AlexPn _ line column) _) = show line ++ ":" ++ show column
}