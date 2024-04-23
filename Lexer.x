{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  FILE                              { \p _ -> Tok p TokFILE                              }
  FIND                              { \p _ -> Tok p TokFIND                              }
  OUT                               { \p _ -> Tok p TokOUT                               }
  IF                                { \p _ -> Tok p TokIf                                }
  ELSE                              { \p _ -> Tok p TokElse                              }
  FOR                               { \p _ -> Tok p TokFor                               }
  ADD                               { \p _ -> Tok p TokAdd                               }
  Graph                             { \p _ -> Tok p TokGType                             }
  Integer                           { \p _ -> Tok p TokIType                             }
  String                            { \p _ -> Tok p TokSType                             }
  Boolean                           { \p _ -> Tok p TokBType                             }
  True                              { \p _ -> Tok p TokTrue                              }
  False                             { \p _ -> Tok p TokFalse                             }
  Node                              { \p _ -> Tok p TokNType                             }
  Relation                          { \p _ -> Tok p TokRType                             }
  ">="                              { \p _ -> Tok p TokGEQ                               }
  "<="                              { \p _ -> Tok p TokLEQ                               }
  \>                                { \p _ -> Tok p TokGT                                }
  \<                                { \p _ -> Tok p TokLT                                }
  \=                                { \p _ -> Tok p TokAssign                            }
  "=="                              { \p _ -> Tok p TokEquals                            }
  \[                                { \p _ -> Tok p TokLSQ                               }
  \]                                { \p _ -> Tok p TokRSQ                               }
  "<-"                              { \p _ -> Tok p TokLArrow                            }
  "->"                              { \p _ -> Tok p TokRArrow                            }
  \-                                { \p _ -> Tok p TokDash                              }
  r\".*\"                           { \p s -> Tok p (Tokrgx (drop 2 $ init s))           }
  \;                                { \p _ -> Tok p TokSemicolon                         }
  \!=                               { \p _ -> Tok p TokNotEquals                         }
  \,                                { \p _ -> Tok p TokComma                             }
  \.                                { \p _ -> Tok p TokDot                               }
  \:[A-Z]+                          { \p s -> Tok p (TokFIdent (drop 1 s))               }
  \'($alpha|$digit|\.)*\'           { \p s -> Tok p (TokString (drop 1 $ init s))        }
  \"($alpha*)+\"                    { \p s -> Tok p (TokStringLiteral (drop 1 $ init s)) }
  \"                                { \p _ -> Tok p TokDelimiter                         }
  "||"                              { \p _ -> Tok p TokOr                                }
  "&&"                              { \p _ -> Tok p TokAnd                               }
  \(                                { \p _ -> Tok p TokBracketLeft                            }
  \)                                { \p _ -> Tok p TokRParen                            }
  \{                                { \p _ -> Tok p TokLCurl                             }
  \}                                { \p _ -> Tok p TokRCurl                             }
  \:                                { \p _ -> Tok p TokColon                             }
  [a-z]($alpha|$digit)*             { \p s -> Tok p (TokIdent s)                         }
  $digit+                           { \p s -> Tok p (TokInt (read s :: Int))             }
  $white+                           ;
  "--".*                            ;

{
data TokenType
  = TokFILE
  | TokFIND
  | TokOUT
  | TokDelimiter
  | TokOr
  | TokAnd
  | TokBracketLeft
  | TokRParen
  | TokGType
  | TokIType
  | TokSType
  | TokStringLiteral String
  | TokBType
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
  | Tokrgx String
  | TokAdd
  | TokDot
  | TokFIdent String
  | TokString String
  | TokTrue
  | TokFalse
  | TokSemicolon
  | TokNotEquals
  | TokComma
  | TokInt Int
  | TokNType
  | TokRType
  deriving (Eq, Show)

data Token = Tok AlexPosn TokenType
  deriving (Eq, Show)

tokenPosn :: Token -> String
tokenPosn (Tok (AlexPn _ line column) _) = show line ++ ":" ++ show column

lex :: String -> [Token]
lex str = alexScanTokens str
} 