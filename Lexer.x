{
module Lexer where
}

%wrapper "posn"

-- Character classes
$n = 0-9
$char = [a-zA-Z]
$sym = [ \. \/ \\ \' \_]

-- Token definitions
tokens :-

  -- Ignore whitespace and comments
  $white+                         ;
  "//".*                          ;

  -- Numeric literals
  $n+                           { \x intChar -> Key (KeyNum (read intChar)) x }

  -- Keywords
  ACCESS                          { \x _ -> Key KeyACCESSToken x }
  CASE                            { \x _ -> Key KeyCASEToken x }
  STDOUT                             { \x _ -> Key KeySTDOUTToken x }
  DataStructure                   { \x _ -> Key KeyDataStructureToken x }
  Num                             { \x _ -> Key KeyNumToken x }
  Chars                           { \x _ -> Key KeyCharsToken x }
  Bool                            { \x _ -> Key KeyBoolToken x }
  CONDIF                              { \x _ -> Key KeyCONDIFToken x }
  CONDELIF                            { \x _ -> Key KeyCONDELIFToken x }
  THROUGH                             { \x _ -> Key KeyTHROUGHToken x }
  DataPoint                            { \x _ -> Key KeyDataPointToken x }
  Association                        { \x _ -> Key KeyAssociationToken x }
  CALLASSOCIATION                     { \x _ -> Key KeyCallAssociationToken x }
  HAS                        { \x _ -> Key KeyHasToken x }
  CALLDATAPOINT                         { \x _ -> Key KeyCallDataPointToken x }
  PLUS                             { \x _ -> Key KeyPlusToken x }
  NOT                         { \x _ -> Key KeyNotToken x }

  -- Operators
  "OR"                            { \x _ -> Key KeyLogicalOr x }
  "AND"                           { \x _ -> Key KeyLogicalAnd x }
  \(                              { \x _ -> Key KeyBracketLeft x }
  \)                              { \x _ -> Key KeyBracketRight x }
  \{                              { \x _ -> Key KeyBraceLeft x }
  \}                              { \x _ -> Key KeyBraceRight x }
  \:                              { \x _ -> Key KeySeparatorColon x }
  ">="                            { \x _ -> Key KeyInequalitySlackGreater x }
  "<="                            { \x _ -> Key KeyInequalitySlackLesser x }
  \>                              { \x _ -> Key KeyInequalityStrictGreater x }
  \<                              { \x _ -> Key KeyInequalityStrictLesser x }
  \=                              { \x _ -> Key KeySet x }
  "i=="                            { \x _ -> Key KeyIdentical x }
  \[                              { \x _ -> Key KeyBracketLeftSquare x }
  \]                              { \x _ -> Key KeyBracketRightSquare x }
  "->"                            { \x _ -> Key KeyDirectionalRight x }
  \-                              { \x _ -> Key KeyNumericMinus x }
  \+                              { \x _ -> Key KeyNumericAdd x }
  \*                              { \x _ -> Key KeyNumericMultiply x }
  \/                              { \x _ -> Key KeyNumericDivide x }
  "=+"                            { \x _ -> Key KeyNumericIncrease x }
  "=-"                            { \x _ -> Key KeyNumericDecrease x }
  \,                              { \x _ -> Key KeySeparatorComma x }
  \;                              { \x _ -> Key KeySeparatorColonSemi x }
  "!=="                            { \x _ -> Key KeyIdenticalNot x }
  \.                              { \x _ -> Key KeyPeriod x }

  -- Identifiers
  [a-z]($char|$n)*            { \x identifier -> Key (KeyIdentity identifier) x }
  r\".*\"                         { \x rational -> Key (KeyRegular $ show $ drop 2 $ take ((length rational) - 1) rational) x }
  \:([A-Z]|\_)+                   { \x header -> Key (KeyHeader $ show $ drop 1 header) x }
  \"($char|$n|$sym)*\"{ \x chars -> Key (KeyChars $ show $ drop 1 $ take ((length chars) - 1) chars) x }
  True                            { \x _ -> Key KeyBoolTrue x }
  False                           { \x _ -> Key KeyBoolFalse x }

{
data Token = Key TokenType AlexPosn
  deriving (Eq, Show)

data TokenType
  = KeyNum Int
  | KeyACCESSToken
  | KeyCASEToken
  | KeySTDOUTToken
  | KeyLogicalOr
  | KeyLogicalAnd
  | KeyBracketLeft
  | KeyBracketRight
  | KeyDataStructureToken
  | KeyNumToken
  | KeyCharsToken
  | KeyBoolToken
  | KeyBraceLeft
  | KeyBraceRight
  | KeyCONDIFToken
  | KeyCONDELIFToken
  | KeyTHROUGHToken
  | KeySeparatorColon
  | KeyIdentity String
  | KeyInequalitySlackGreater
  | KeyInequalitySlackLesser
  | KeyInequalityStrictGreater
  | KeyInequalityStrictLesser
  | KeySet
  | KeyIdentical
  | KeyBracketLeftSquare
  | KeyBracketRightSquare
  | KeyDirectionalRight
  | KeyNumericMinus
  | KeyRegular String
  | KeyPlusToken
  | KeyPeriod
  | KeyHeader String
  | KeyChars String
  | KeyBoolTrue
  | KeyBoolFalse
  | KeySeparatorColonSemi
  | KeyIdenticalNot
  | KeySeparatorComma
  | KeyDataPointToken
  | KeyAssociationToken
  | KeyCallAssociationToken
  | KeyHasToken
  | KeyCallDataPointToken
  | KeyNumericAdd
  | KeyNumericMultiply
  | KeyNumericDivide
  | KeyNumericIncrease
  | KeyNumericDecrease
  | KeyNotToken
  deriving (Eq, Show)
}