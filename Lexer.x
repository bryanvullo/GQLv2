{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$symbol = [ \. \/ \\ \' \_ \^ \( \) \> \< \$ \? \| \[ \] \{ \} \* \+]

tokens :-
  $white+                       ;
  "#".*                         ;

  -- Keywords
  ACCESS                        { \p _ -> Key KeyACCESSToken p }
  CASE                          { \p _ -> Key KeyCaseToken p }
  STDOUT                        { \p _ -> Key KeySTDOUTToken p }
  AND                           { \p _ -> Key KeyLogicalAnd p }
  OR                            { \p _ -> Key KeyLogicalOr p }
  Graph                         { \p _ -> Key KeyGraphTypeToken p }
  Integer                       { \p _ -> Key KeyIntegerTypeToken p }
  String                        { \p _ -> Key KeyStringTypeToken p }
  Boolean                       { \p _ -> Key KeyBooleanTypeToken p }
  Node                          { \p _ -> Key KeyNodeTypeToken p }
  Relation                      { \p _ -> Key KeyRelationTypeToken p }
  HAS                           { \p _ -> Key KeyHasToken p }
  CONDIF                        { \p _ -> Key KeyCONDIFToken p }
  CONDELIF                      { \p _ -> Key KeyCONDELIFToken p }
  THROUGH                       { \p _ -> Key KeyTHROUGHToken p }
  NEGATE                        { \p _ -> Key KeyNegateToken p }
  DataPoint                     { \p _ -> Key KeyDataPointToken p }
  Association                   { \p _ -> Key KeyAssociationToken p }
  CALLASSOCIATION               { \p _ -> Key KeyCallAssocToken p }
  CALLDATAPOINT                 { \p _ -> Key KeyCallDataToken p }
  CHARS                         { \p _ -> Key KeyCharsType p }
  PLUS                          { \p _ -> Key KeyPlusToken p }
  SUBT                          { \p _ -> Key KeySubtractToken p }
  MULT                          { \p _ -> Key KeyMultiplyToken p }
  DIV                           { \p _ -> Key KeyDivideToken p }
  True                          { \p _ -> Key KeyTrue p }
  False                         { \p _ -> Key KeyFalse p }

  -- Operators and punctuation
  \(                            { \p _ -> Key KeyBracketLeft p }
  \)                            { \p _ -> Key KeyBracketRight p }
  ">>"                          { \p _ -> Key KeyGreaterEqual p }
  "<<"                          { \p _ -> Key KeyLessEqual p }
  ">"                           { \p _ -> Key KeyGreater p }
  "<"                           { \p _ -> Key KeyLess p }
  "="                           { \p _ -> Key KeyAssign p }
  "i=="                         { \p _ -> Key KeyEqual p }
  "!=="                         { \p _ -> Key KeyNotEqual p }
  "."                           { \p _ -> Key KeyDot p }
  "-"                           { \p _ -> Key KeyHyphen p }
  "^"                           { \p _ -> Key KeyEdge p }
  "{"                           { \p _ -> Key KeyBraceLeft p }
  "}"                           { \p _ -> Key KeyBraceRight p }
  ":"                           { \p _ -> Key KeyColon p }
  "+="                          { \p _ -> Key KeyIncToken p }
  "-="                          { \p _ -> Key KeyDecToken p }

  -- Literals
  $digit+                       { \p s -> Key (KeyNum $ read s) p }
  \"($alpha|$digit|$symbol)*\"  { \p s -> Key (KeyChar $ drop 1 $ (take ((length s) - 1)) s) p }
  [a-z]($alpha|$digit)*         { \p s -> Key (KeyArgument s) p }
  \:([A-Z]|\_)+                 { \p s -> Key (KeyHeaderToken $ drop 1 s) p }
  r\"($digit|$alpha|$symbol)*\" { \p s -> Key (KeyRegularToken $ drop 2 $ (take ((length s) - 1)) s) p }

{

data Token
  = Key TokenType AlexPosn
  deriving (Eq, Show)

data TokenType
  = KeyACCESSToken
  | KeyCaseToken
  | KeySTDOUTToken
  | KeyLogicalAnd
  | KeyLogicalOr
  | KeyBracketLeft
  | KeyBracketRight
  | KeyGraphTypeToken
  | KeyIntegerTypeToken
  | KeyStringTypeToken
  | KeyBooleanTypeToken
  | KeyNodeTypeToken
  | KeyRelationTypeToken
  | KeyHasToken
  | KeyCONDIFToken
  | KeyCONDELIFToken
  | KeyTHROUGHToken
  | KeyNegateToken
  | KeyGreaterEqual
  | KeyLessEqual
  | KeyGreater
  | KeyLess
  | KeyAssign
  | KeyEqual
  | KeyNotEqual
  | KeyPlusToken
  | KeySubtractToken
  | KeyMultiplyToken
  | KeyDivideToken
  | KeyDot
  | KeyTrue
  | KeyFalse
  | KeyHyphen
  | KeyDataPointToken
  | KeyAssociationToken
  | KeyCallAssocToken
  | KeyCallDataToken
  | KeyEdge
  | KeyNum Int
  | KeyHeaderToken String
  | KeyChar String
  | KeyCharsType
  | KeyIdentity String
  | KeyArgument String
  | KeyRegularToken String
  | KeyBraceLeft
  | KeyBraceRight
  | KeyColon
  | KeyIncToken
  | KeyDecToken
  deriving (Eq, Show)

}