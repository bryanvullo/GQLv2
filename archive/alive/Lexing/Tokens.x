{
module Lexing.Tokens where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$symbol = [ \. \/ \\ \' \_ \^ \( \) \> \< \$ \? \| \[ \] \{ \} \* \+]

tokens :-
 $white+                         ;
 "//".*                          ;
 $digit+                         { \p s -> Tn (TokenInt $ read s) p }
 READFILE                        { \p _ -> Tn TokenReadFile p }
 MATCH                           { \p _ -> Tn TokenMatch p }
 PRINT                           { \p _ -> Tn TokenPrint p }
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
 "->"                            { \p _ -> Tn TokenRArrow p }
 \-                              { \p _ -> Tn TokenDash p }
 r\"($digit|$alpha|$symbol)*\"   { \p s -> Tn (TokenRegex $ drop 2 $ (take ((length s) - 1)) s) p }
 ADD                             { \p _ -> Tn TokenAdd p }
 \.                              { \p _ -> Tn TokenDot p }
 \:([A-Z]|\_)+                   { \p s -> Tn (TokenBigField $ drop 1 s) p }
 \"($alpha|$digit|$symbol)*\"    { \p s -> Tn (TokenString $ drop 1 $ (take ((length s) - 1)) s) p }
 True                            { \p _ -> Tn TokenTrue p }
 False                           { \p _ -> Tn TokenFalse p }
 \;                              { \p _ -> Tn TokenSemicolon p }
 "!="                            { \p _ -> Tn TokenNotEquals p }
 \,                              { \p _ -> Tn TokenComma p }
 Node                            { \p _ -> Tn TokenNodeType p }
 Relation                        { \p _ -> Tn TokenRelationType p }
 GETRELATION                     { \p _ -> Tn TokenGetRelation p }
 CONTAINS                        { \p _ -> Tn TokenContains p }
 GETNODE                         { \p _ -> Tn TokenGetNode p }
 \+                              { \p _ -> Tn TokenPlus p }
 \*                              { \p _ -> Tn TokenStar p }
 \/                              { \p _ -> Tn TokenFSlash p }
 "+="                            { \p _ -> Tn TokenIncrement p }
 "-="                            { \p _ -> Tn TokenDecrement p }
 EXCLUDE                         { \p _ -> Tn TokenExclude p }

{
data Token = Tn TokenBase AlexPosn
  deriving (Eq, Show)

data TokenBase =
 TokenInt Int |
 TokenReadFile |
 TokenMatch |
 TokenPrint |
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
 TokenRArrow |
 TokenDash |
 TokenRegex String |
 TokenAdd |
 TokenDot |
 TokenBigField String |
 TokenString String |
 TokenTrue |
 TokenFalse |
 TokenSemicolon |
 TokenNotEquals |
 TokenComma |
 TokenNodeType |
 TokenRelationType |
 TokenGetRelation |
 TokenContains |
 TokenGetNode |
 TokenPlus |
 TokenStar |
 TokenFSlash |
 TokenIncrement |
 TokenDecrement |
 TokenExclude
  deriving (Eq, Show)
}
