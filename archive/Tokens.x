{
module Lexing.Tokens where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
 $white+                         ;
 "//".*                          ;
 $digit+                         { \p s -> Tn (TokenInt $ read s) p }
 READFILE                        { \p _ -> Tn TokenReadFile p }
 MATCH                           { \p _ -> Tn TokenMatch p }
 PRINT                           { \p _ -> Tn TokenPrint p }
 \"                              { \p _ -> Tn TokenDelimiter p }
 "||"                            { \p _ -> Tn TokenOr p }
 "&&"                            { \p _ -> Tn TokenAnd p }
 \(                              { \p _ -> Tn TokenLParen p }
 \)                              { \p _ -> Tn TokenRParen p }
 Graph                           { \p _ -> Tn TokenGraphType p }
 Integer                         { \p _ -> Tn TokenIntegerType p }
 String                          { \p _ -> Tn TokenStringType p }
 Boolean                         { \p _ -> Tn TokenBooleanType p }
 \{                              { \p _ -> Tn TokenLCurl p }
 \}                              { \p _ -> Tn TokenRCurl p }
 IF                              { \p _ -> Tn TokenIf p }
 ELSE                            { \p _ -> Tn TokenElse p }
 FOR                             { \p _ -> Tn TokenFor p }
 \:                              { \p _ -> Tn TokenColon p }
 [a-z]($alpha|$digit)*           { \p s -> Tn (TokenVar s) p }
 ">="                            { \p _ -> Tn TokenGEQ p }
 "<="                            { \p _ -> Tn TokenLEQ p }
 \>                              { \p _ -> Tn TokenGT p }
 \<                              { \p _ -> Tn TokenLT p }
 \=                              { \p _ -> Tn TokenAssign p }
 "=="                            { \p _ -> Tn TokenEquals p }
 \[                              { \p _ -> Tn TokenLSQ p }
 \]                              { \p _ -> Tn TokenRSQ p }
 "<-"                            { \p _ -> Tn TokenLArrow p }
 "->"                            { \p _ -> Tn TokenRArrow p }
 \-                              { \p _ -> Tn TokenDash p }
 r\".*\"                         { \p s -> Tn (TokenRegex $ show $ drop 2 $ (take ((length s) - 1)) s) p }
 ADD                             { \p _ -> Tn TokenAdd p }
 \.                              { \p _ -> Tn TokenDot p }
 \:[A-Z]+                        { \p s -> Tn (TokenBigField $ show $ drop 1 s) p }
 \"$alpha*\"                     { \p s -> Tn (TokenString $ show $ drop 1 $ (take ((length s) - 1)) s) p }
 True                            { \p _ -> Tn TokenTrue p }
 False                           { \p _ -> Tn TokenFalse p }
 \;                              { \p _ -> Tn TokenSemicolon p }

{
data Token = Tn TokenBase AlexPosn
  deriving (Eq, Show)

data TokenBase =
 TokenInt Int |
 TokenReadFile |
 TokenMatch |
 TokenPrint |
 TokenDelimiter |
 TokenOr |
 TokenAnd |
 TokenLParen |
 TokenRParen |
 TokenGraphType |
 TokenIntegerType |
 TokenStringType |
 TokenBooleanType |
 TokenLCurl |
 TokenRCurl |
 TokenIf |
 TokenElse |
 TokenFor |
 TokenColon |
 TokenVar String |
 TokenGEQ |
 TokenLEQ |
 TokenGT |
 TokenLT |
 TokenAssign |
 TokenEquals |
 TokenLSQ |
 TokenRSQ |
 TokenLArrow |
 TokenRArrow |
 TokenDash |
 TokenRegex String |
 TokenAdd |
 TokenDot |
 TokenBigField String |
 TokenString String |
 TokenTrue |
 TokenFalse |
 TokenSemicolon
  deriving (Eq, Show)
}
