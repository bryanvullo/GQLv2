{
module Lexing.N4jLexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$symbol = [\_ \;]

tokens :-
  $white+                         ;
  null                            { \p _ -> Tn4j TokenNull p }
  [\-]?[0-9]+                     { \p s -> Tn4j (TokenInt $ read s) p }
  [\+][0-9]+                      { \p s -> Tn4j (TokenInt $ read (drop 1 s)) p }
  \"($alpha|$digit|$symbol)+\"    { \p s -> Tn4j (TokenString $ show $ drop 1 $ (take ((length s) - 1)) s) p }
  ($alpha|$symbol|$digit)+        { \p s -> Tn4j (TokenField $ s) p }
  \,                              { \p _ -> Tn4j TokenComma p }
  \:                              { \p _ -> Tn4j TokenColon p }
  true                            { \p _ -> Tn4j TokenTrue p }
  false                           { \p _ -> Tn4j TokenFalse p }
  

{
data Token = Tn4j TokenBase AlexPosn
  deriving (Eq, Show)

data TokenBase =
 TokenString String |
 TokenInt Int |
 TokenTrue |
 TokenFalse |
 TokenComma |
 TokenColon |
 TokenID |
 TokenField String |
 TokenNull
  deriving (Eq, Show)
}
