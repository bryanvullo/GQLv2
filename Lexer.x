{
module Lexer where
}

%wrapper "posn"

-- Character sets
$digit  = 0-9                                                          -- Digits
$alpha  = [a-zA-Z]                                                     -- Alphabetic characters
$symbol = [ \. \/ \\ \' \_ \^ \( \) \> \< \$ \? \| \[ \] \{ \} \* \+]  -- Symbols

tokens :-
  -- Whitespace and comments
  $white+                       ;  -- Ignore whitespace
  "#".*                         ;  -- Ignore comments

  -- Keywords
  ACCESS                        { \x _ -> Key XACCESS           x }  -- Access keyword
  CASE                          { \x _ -> Key XCASE             x }  -- Case keyword
  STDOUT                        { \x _ -> Key XSTDOUT           x }  -- Stdout keyword
  AND                           { \x _ -> Key XLogicalAND       x }  -- Logical AND
  OR                            { \x _ -> Key XLogicalOR        x }  -- Logical OR
  DataGraph                     { \x _ -> Key XDataGraph        x }  -- DataGraph keyword
  Integer                       { \x _ -> Key XInt              x }  -- Integer keyword
  String                        { \x _ -> Key XStr              x }  -- String keyword
  Boolean                       { \x _ -> Key XBool             x }  -- Boolean keyword
  Node                          { \x _ -> Key XNode             x }  -- Node keyword
  Relation                      { \x _ -> Key XDataAssociation  x }  -- Relation keyword
  HAS                           { \x _ -> Key XHAS              x }  -- Has keyword
  CONDIF                        { \x _ -> Key XCONDIF           x }  -- If keyword
  CONDELIF                      { \x _ -> Key XCONDELIF         x }  -- Else if keyword
  THROUGH                       { \x _ -> Key XTHROUGH          x }  -- Through keyword
  NEGATE                        { \x _ -> Key XNegate           x }  -- Negate keyword
  DataPoint                     { \x _ -> Key XDataPoint        x }  -- DataPoint keyword
  Association                   { \x _ -> Key XAssociation      x }  -- Association keyword
  CALLASSOCIATION               { \x _ -> Key XAssociationCheck x }  -- CallAssociation keyword
  CALLDATAPOINT                 { \x _ -> Key XDataNodeCheck    x }  -- CallDataPoint keyword
  CHARS                         { \x _ -> Key XCharsType        x }  -- Chars keyword
  PLUS                          { \x _ -> Key XPlus             x }  -- Plus keyword
  SUBT                          { \x _ -> Key XSubtract         x }  -- Subtract keyword
  MULT                          { \x _ -> Key XMultiply         x }  -- Multiply keyword
  DIV                           { \x _ -> Key XDivide           x }  -- Divide keyword
  True                          { \x _ -> Key XBooleanTrue      x }  -- Boolean true
  False                         { \x _ -> Key XBooleanFalse     x }  -- Boolean false

  -- Operators and punctuation
  \(                            { \x _ -> Key XBracketLeft        x }  -- Left parenthesis
  \)                            { \x _ -> Key XBracketRight       x }  -- Right parenthesis
  ">>"                          { \x _ -> Key XSlackGreater       x }  -- Slack greater than
  "<<"                          { \x _ -> Key XSlackLesser        x }  -- Slack less than
  ">"                           { \x _ -> Key XStrictGreater      x }  -- Strict greater than
  "<"                           { \x _ -> Key XStrictLesser       x }  -- Strict less than
  "="                           { \x _ -> Key XAssign                x }  -- Assignment operator
  "i=="                         { \x _ -> Key XLogicalEquation    x }  -- Logical equality
  "!=="                         { \x _ -> Key XLogicalInequation  x }  -- Logical inequality
  "."                           { \x _ -> Key XPeriod             x }  -- Period
  "-"                           { \x _ -> Key XHyphen             x }  -- Hyphen
  "^"                           { \x _ -> Key XAssociationSymbol  x }  -- Association symbol
  "{"                           { \x _ -> Key XBraceLeft          x }  -- Left brace
  "}"                           { \x _ -> Key XBraceRight         x }  -- Right brace
  ":"                           { \x _ -> Key XColonSymbol        x }  -- Colon symbol
  "++"                          { \x _ -> Key XNumericalIncrement x }  -- Numerical increment
  "--"                          { \x _ -> Key XNumericalDecrement x }  -- Numerical decrement

  -- Literals
  $digit+                       { \x y -> Key (XNum $ read y)                                         x }  -- Integer literal
  \"($alpha|$digit|$symbol)*\"  { \x y -> Key (XChar $ drop 1 $ take ((length y) - 1) y)              x }  -- Character literal
  [a-z]($alpha|$digit)*         { \x y -> Key (XArgument y)                                           x }  -- Argument literal
  \:([A-Z]|\_)+                 { \x y -> Key (XHeader $ drop 1 y)                                    x }  -- Header literal
  r\"($digit|$alpha|$symbol)*\" { \x y -> Key (XRegularExpression $ drop 2 $ take ((length y) - 1) y) x }  -- Regular expression literal

{

data Token
  = Key TokenType AlexPosn
  deriving (Eq, Show)

data TokenType
  -- Keywords
  = XACCESS
  | XCASE
  | XSTDOUT
  | XLogicalAND
  | XLogicalOR
  | XDataGraph
  | XInt
  | XStr
  | XBool
  | XNode
  | XDataAssociation
  | XHAS
  | XCONDIF
  | XCONDELIF
  | XTHROUGH
  | XNegate
  | XDataPoint
  | XAssociation
  | XAssociationCheck
  | XDataNodeCheck
  | XCharsType
  | XPlus
  | XSubtract
  | XMultiply
  | XDivide
  | XBooleanTrue
  | XBooleanFalse
  
  -- Operators and punctuation  
  | XBracketLeft
  | XBracketRight
  | XSlackGreater
  | XSlackLesser
  | XStrictGreater
  | XStrictLesser
  | XAssign
  | XLogicalEquation
  | XLogicalInequation
  | XPeriod
  | XHyphen
  | XAssociationSymbol
  | XBraceLeft
  | XBraceRight
  | XColonSymbol
  | XNumericalIncrement
  | XNumericalDecrement
  
  -- Literals
  | XNum Int
  | XHeader String
  | XChar String
  | XIdentity String
  | XArgument String
  | XRegularExpression String
  deriving (Eq, Show)

}