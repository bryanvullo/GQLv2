{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+                           ;
  "--".*                            ;
  $digit+                           { \p s -> Tok p (TokInt (read s :: Integer)) }
  READFILE                          { \p _ -> Tok p TokReadFile }
  MATCH                             { \p _ -> Tok p TokMatch }
  PRINT                             { \p _ -> Tok p TokPrint }
  \"                                { \p _ -> Tok p TokDelimiter }
  "||"                              { \p _ -> Tok p TokOr }
  "&&"                              { \p _ -> Tok p TokAnd }
  \(                                { \p _ -> Tok p TokLParen }
  \)                                { \p _ -> Tok p TokRParen }
  Graph                             { \p _ -> Tok p TokGraphType }
  Integer                           { \p _ -> Tok p TokIntegerType }
  String                            { \p _ -> Tok p TokStringType }
  Boolean                           { \p _ -> Tok p TokBooleanType }
  \{                                { \p _ -> Tok p TokLCurl }
  \}                                { \p _ -> Tok p TokRCurl }
  IF                                { \p _ -> Tok p TokIf }
  ELSE                              { \p _ -> Tok p TokElse }
  FOR                               { \p _ -> Tok p TokFor }
  \:                                { \p _ -> Tok p TokColon }
  [a-z]($alpha|$digit)*             { \p s -> Tok p (TokIdent s) }
  ">="                              { \p _ -> Tok p TokGEQ }
  "<="                              { \p _ -> Tok p TokLEQ }
  \>                                { \p _ -> Tok p TokGT }
  \<                                { \p _ -> Tok p TokLT }
  \=                                { \p _ -> Tok p TokAssign }
  "=="                              { \p _ -> Tok p TokEquals }
  \[                                { \p _ -> Tok p TokLSQ }
  \]                                { \p _ -> Tok p TokRSQ }
  "<-"                              { \p _ -> Tok p TokLArrow }
  "->"                              { \p _ -> Tok p TokRArrow }
  \-                                { \p _ -> Tok p TokDash }
  r\".*\"                           { \p s -> Tok p (TokRegex (drop 2 $ init s)) }
  ADD                               { \p _ -> Tok p TokAdd }
  \.                                { \p _ -> Tok p TokDot }
  \:[A-Z]+                          { \p s -> Tok p (TokBigField (drop 1 s)) }
  \"($alpha*)+\"                    { \p s -> Tok p (TokString (drop 1 $ init s)) }
  True                              { \p _ -> Tok p TokTrue }
  False                             { \p _ -> Tok p TokFalse }
  \;                                { \p _ -> Tok p TokSemicolon }
  \!=                               { \p _ -> Tok p TokNotEquals }
  \,                                { \p _ -> Tok p TokComma }
  Node                              { \p _ -> Tok p TokNodeType }
  Relation                          { \p _ -> Tok p TokRelationType }

{
data TokenType
  = TokReadFile
  | TokMatch
  | TokPrint
  | TokDelimiter
  | TokOr
  | TokAnd
  | TokLParen
  | TokRParen
  | TokGraphType
  | TokIntegerType
  | TokStringType
  | TokBooleanType
  | TokLCurl
  | TokRCurl
  | TokIf
  | TokElse
  | TokFor
  | TokColon
  | TokIdent String
  | TokGEQ
  | TokLEQ
  | TokGT
  | TokLT
  | TokAssign
  | TokEquals
  | TokLSQ
  | TokRSQ
  | TokLArrow
  | TokRArrow
  | TokDash
  | TokRegex String
  | TokAdd
  | TokDot
  | TokBigField String
  | TokString String
  | TokTrue
  | TokFalse
  | TokSemicolon
  | TokNotEquals
  | TokComma
  | TokInt Integer
  | TokNodeType
  | TokRelationType
  deriving (Eq, Show)

data Token = Tok AlexPosn TokenType
  deriving (Eq, Show)

tokenPosn :: Token -> String
tokenPosn (Tok (AlexPn _ line column) _) = show line ++ ":" ++ show column

lex :: String -> [Token]
lex str = alexScanTokens str
}