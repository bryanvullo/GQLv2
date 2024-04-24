{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]  
$eol   = [\n]

tokens :-

  $eol                                  ;
  $white+                               ;
  "#".*                                 ;
  $digit+                               { \p s -> Key (KeyNum (read s)) p }
  ACCESS                                { \p _ -> Key KeyACCESSToken p }
  CASE                                  { \p _ -> Key KeyCASEToken p }
  STDOUT                                { \p _ -> Key KeySTDOUTToken p }
  "AND"                                 { \p _ -> Key KeyLogicalAnd p }
  "OR"                                  { \p _ -> Key KeyLogicalOr p }
  "("                                   { \p _ -> Key KeyBracketLeft p }
  ")"                                   { \p _ -> Key KeyBracketRight p }
  DataStructure                         { \p _ -> Key KeyDataStructureToken p }
  "{"                                   { \p _ -> Key KeyBraceLeft p }
  "}"                                   { \p _ -> Key KeyBraceRight p }
  CONDIF                                { \p _ -> Key KeyCONDIFToken p }
  THROUGH                               { \p _ -> Key KeyTHROUGHToken p }
  $alpha [$alpha $digit \_ \']*         { \p s -> Key (KeyIdentity s) p }
  "<="                                  { \p _ -> Key KeyInequalitySlackLesser p }
  "="                                   { \p _ -> Key KeySet p }
  "."                                   { \p _ -> Key KeyPeriod p }
  r\" ([^\"\\]|\\.)*  \"                { \p s -> Key (KeyRegular (read s)) p }
  PLUS                                  { \p _ -> Key KeyPlusToken p }
  ":" $alpha+                           { \p s -> Key (KeyHeader (drop 1 s)) p }
  \" ([^\"\\]|\\.)*  \"                 { \p s -> Key (KeyChars (read s)) p }
  True                                  { \p _ -> Key KeyBoolTrue p }
  False                                 { \p _ -> Key KeyBoolFalse p }
  ";"                                   { \p _ -> Key KeySeparatorColonSemi p }
  "!=="                                 { \p _ -> Key KeyIdenticalNot p }
  HAS                                   { \p _ -> Key KeyHasToken p }
  CALLASSOCIATION                       { \p _ -> Key KeyCallAssociationToken p }
  CALLDATAPOINT                         { \p _ -> Key KeyCallDataPointToken p }
  NOT                                   { \p _ -> Key KeyNotToken p }
  "+"                                   { \p _ -> Key KeyNumericPlus p }
  "-"                                   { \p _ -> Key KeyNumericMinus p }
  ">="                                  { \p _ -> Key KeyInequalitySlackGreater p }
  DataPoint                             { \p _ -> Key KeyDataPointToken p }

{

data Token = Key TokenType AlexPosn
  deriving (Eq, Show)

data TokenType
  = KeyNum Int
  | KeyACCESSToken
  | KeyCASEToken  
  | KeySTDOUTToken
  | KeyLogicalAnd
  | KeyLogicalOr
  | KeyBracketLeft
  | KeyBracketRight
  | KeyDataStructureToken
  | KeyBraceLeft
  | KeyBraceRight  
  | KeyCONDIFToken
  | KeyTHROUGHToken
  | KeyIdentity String
  | KeyInequalitySlackLesser
  | KeySet
  | KeyPeriod
  | KeyRegular String
  | KeyPlusToken
  | KeyHeader String
  | KeyChars String
  | KeyBoolTrue
  | KeyBoolFalse
  | KeySeparatorColonSemi
  | KeyIdenticalNot
  | KeyHasToken
  | KeyCallAssociationToken
  | KeyCallDataPointToken
  | KeyNotToken  
  | KeyNumericPlus
  | KeyNumericMinus
  | KeyInequalitySlackGreater
  | KeyDataPointToken
  deriving (Eq, Show)

}