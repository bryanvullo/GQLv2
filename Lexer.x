{
module Lexer where
}

%wrapper "posn"

-- Character classes
$num     = 0-9                 -- Digits
$alpha   = [a-zA-Z]            -- Alphabetic characters
$alnum   = [$alpha $num]       -- Alphanumeric characters
$sym     = [ \. \/ \\ \' \_]   -- Symbols allowed in identifiers
$graphic = [$alnum $sym]       -- All valid characters for identifiers

-- Token definitions
tokens :-

  -- Whitespace and comments are ignored
  $white+                              ;
  "#".*                                ;

  -- Numeric literals
  $num+                                { \x y -> Key (KeyNum (read y)) x             } -- Numeric literal

  -- Keywords for language constructs and data types
  ACCESS                               { \x _ -> Key KeyACCESSToken x                } -- Keyword for accessing input files
  CASE                                 { \x _ -> Key KeyCASEToken x                  } -- Keyword for case statements
  STDOUT                               { \x _ -> Key KeySTDOUTToken x                } -- Keyword for standard output
  DataStructure                        { \x _ -> Key KeyDataStructureToken x         } -- Keyword for the Graph data structure
  Num                                  { \x _ -> Key KeyNumToken x                   } -- Keyword for numeric data type
  Chars                                { \x _ -> Key KeyCharsToken x                 } -- Keyword for string data type
  Bool                                 { \x _ -> Key KeyBoolToken x                  } -- Keyword for boolean data type 
  CONDIF                               { \x _ -> Key KeyCONDIFToken x                } -- Keyword for conditional if statements
  CONDELIF                             { \x _ -> Key KeyCONDELIFToken x              } -- Keyword for conditional else-if statements
  THROUGH                              { \x _ -> Key KeyTHROUGHToken x               } -- Keyword for loops over data
  DataPoint                            { \x _ -> Key KeyDataPointToken x             } -- Keyword for nodes in the graph
  Association                          { \x _ -> Key KeyAssociationToken x           } -- Keyword for edges in the graph
  CALLASSOCIATION                      { \x _ -> Key KeyCallAssociationToken x       } -- Keyword for querying edges
  HAS                                  { \x _ -> Key KeyHasToken x                   } -- Keyword for property queries on nodes/edges
  CALLDATAPOINT                        { \x _ -> Key KeyCallDataPointToken x         } -- Keyword for querying nodes
  PLUS                                 { \x _ -> Key KeyPlusToken x                  } -- Keyword for adding nodes/edges to output
  NOT                                  { \x _ -> Key KeyNotToken x                   } -- Keyword for excluding nodes/edges from output

  -- Operators
  "OR"                                 { \x _ -> Key KeyLogicalOr x                  } -- Logical OR operator
  "AND"                                { \x _ -> Key KeyLogicalAnd x                 } -- Logical AND operator
  "NEGATE"                             { \x _ -> Key KeyLogicalNegate x              } -- Logical negation operator
  "("                                  { \x _ -> Key KeyBracketLeft x                } -- Left parenthesis
  ")"                                  { \x _ -> Key KeyBracketRight x               } -- Right parenthesis
  "{"                                  { \x _ -> Key KeyBraceLeft x                  } -- Left brace
  "}"                                  { \x _ -> Key KeyBraceRight x                 } -- Right brace
  ":"                                  { \x _ -> Key KeySeparatorColon x             } -- Colon separator
  ">="                                 { \x _ -> Key KeyInequalitySlackGreater x     } -- Greater than or equal to
  "<="                                 { \x _ -> Key KeyInequalitySlackLesser x      } -- Less than or equal to 
  ">"                                  { \x _ -> Key KeyInequalityStrictGreater x    } -- Strictly greater than
  "<"                                  { \x _ -> Key KeyInequalityStrictLesser x     } -- Strictly less than
  "="                                  { \x _ -> Key KeySet x                        } -- Variable assignment  
  "i=="                                { \x _ -> Key KeyIdentical x                  } -- Strict equality test
  "["                                  { \x _ -> Key KeyBracketLeftSquare x          } -- Left square bracket
  "]"                                  { \x _ -> Key KeyBracketRightSquare x         } -- Right square bracket  
  ">>"                                 { \x _ -> Key KeyDirectionalRight x                    } -- Right arrow for edges
  "-"                                  { \x _ -> Key KeyNumericMinus x               } -- Subtraction operator
  "+"                                  { \x _ -> Key KeyNumericPlus x                } -- Addition operator
  "*"                                  { \x _ -> Key KeyNumericMultiply x            } -- Multiplication operator
  "/"                                  { \x _ -> Key KeyNumericDivide x              } -- Division operator
  "=+"                                 { \x _ -> Key KeyNumericIncrease x            } -- Increment operator
  "=-"                                 { \x _ -> Key KeyNumericDecrease x            } -- Decrement operator
  ","                                  { \x _ -> Key KeySeparatorComma x             } -- Comma separator (e.g. for CSVs) 
  ";"                                  { \x _ -> Key KeySeparatorColonSemi x         } -- Semicolon separator (e.g. end of statement)
  "!=="                                { \x _ -> Key KeyIdenticalNot x               } -- Strict inequality test
  "."                                  { \x _ -> Key KeyPeriod x                     } -- Period for attribute access

  -- Identifiers, strings, and other literals 
  [a-z] [$alnum]*                      { \x y -> Key (KeyIdentity y) x               } -- Identifier starting with lowercase letter
  r\" ([^\"\\]|\\.)*  \"               { \x y -> Key (KeyRegular (read y)) x         } -- Regular expression literal
  ":" [$alpha _]+                      { \x y -> Key (KeyHeader (drop 1 y)) x        } -- Header name starting with colon
  \" ($graphic # \")* \"               { \x y -> Key (KeyChars (read y)) x           } -- String literal  
  True                                 { \x _ -> Key KeyBoolTrue x                   } -- Boolean literal "True"
  False                                { \x _ -> Key KeyBoolFalse x                  } -- Boolean literal "False"

{

-- Token type
data Token = Key TokenType AlexPosn
  deriving (Eq, Show)

-- Token types, corresponding to the tokens defined above  
data TokenType
  = KeyNum Int
  | KeyACCESSToken
  | KeyCASEToken
  | KeySTDOUTToken
  | KeyLogicalOr
  | KeyLogicalAnd
  | KeyLogicalNegate
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
  | KeyNumericPlus
  | KeyNumericMultiply
  | KeyNumericDivide
  | KeyNumericIncrease  
  | KeyNumericDecrease
  | KeyNotToken
  deriving (Eq, Show)

}