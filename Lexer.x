{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  $eol                          ;
  $white+                       ;
  "#".*                         ;

  -- Keywords
  ACCESS                        { \p _ -> Key KeyACCESSToken p }
  CASE                          { \p _ -> Key KeyCASEToken p }
  STDOUT                        { \p _ -> Key KeySTDOUTToken p }
  AND                           { \p _ -> Key KeyLogicalAnd p }
  OR                            { \p _ -> Key KeyLogicalOr p }
  Graph                         { \p _ -> Key KeyGraphToken p }
  HAS                           { \p _ -> Key KeyHasToken p }
  CONDIF                        { \p _ -> Key KeyCONDIFToken p }
  THROUGH                       { \p _ -> Key KeyTHROUGHToken p }
  DataPoint                     { \p _ -> Key KeyDataPointToken p }
  Association                   { \p _ -> Key KeyAssociationToken p }
  CALLASSOCIATION               { \p _ -> Key KeyCallAssocToken p }
  CALLDATAPOINT                 { \p _ -> Key KeyCallDataToken p }
  PLUS                          { \p _ -> Key KeyPlusToken p }
  True                          { \p _ -> Key KeyTrue p }
  False                         { \p _ -> Key KeyFalse p }

  -- Operators and punctuation
  \(                            { \p _ -> Key KeyBracketLeft p }
  \)                            { \p _ -> Key KeyBracketRight p }
  ">="                          { \p _ -> Key KeyGreaterEqual p }
  "<="                          { \p _ -> Key KeyLessEqual p }
  ">"                           { \p _ -> Key KeyGreater p }
  "<"                           { \p _ -> Key KeyLess p }
  "="                           { \p _ -> Key KeyAssign p }
  "i=="                         { \p _ -> Key KeyEqual p }
  "!=="                         { \p _ -> Key KeyNotEqual p }
  "."                           { \p _ -> Key KeyDot p }
  "-"                           { \p _ -> Key KeyMinus p }
  "^"                           { \p _ -> Key KeyEdge p }
  "{"                           { \p _ -> Key KeyBraceLeft p }
  "}"                           { \p _ -> Key KeyBraceRight p }
  ":$alpha+"                    { \p s -> Key (KeyColonIdentifier s) p }
  ":"                           { \p _ -> Key KeyColon p }

  -- Literals
  $digit+                       { \p s -> Key (KeyNum (read s)) p }
  \"($printable # \")*\"        { \p s -> Key (KeyChars (read s)) p }
  $alpha [$alpha $digit \_ \']* { \p s -> Key (KeyIdentity s) p }

{

data Token
  = Key TokenType AlexPosn
  deriving (Eq, Show)

data TokenType
  = KeyACCESSToken
  | KeyCASEToken
  | KeySTDOUTToken
  | KeyLogicalAnd
  | KeyLogicalOr
  | KeyBracketLeft
  | KeyBracketRight
  | KeyGraphToken
  | KeyHasToken
  | KeyCONDIFToken
  | KeyTHROUGHToken
  | KeyGreaterEqual
  | KeyLessEqual
  | KeyGreater
  | KeyLess
  | KeyAssign
  | KeyEqual
  | KeyNotEqual
  | KeyPlusToken
  | KeyDot
  | KeyTrue
  | KeyFalse
  | KeyMinus
  | KeyDataPointToken
  | KeyAssociationToken
  | KeyCallAssocToken
  | KeyCallDataToken
  | KeyEdge
  | KeyNum Int
  | KeyChars String
  | KeyIdentity String
  | KeyBraceLeft
  | KeyBraceRight
  | KeyColon
  | KeyColonIdentifier String
  deriving (Eq, Show)

}