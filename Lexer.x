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
  IF                                { \p _ -> Tok p TokCond                              }
  ELSE                              { \p _ -> Tok p TokElse                              }
  FOR                               { \p _ -> Tok p TokFor                               }
  ADD                               { \p _ -> Tok p TokArith                               }
  Graph                             { \p _ -> Tok p TokGr                             }
  Integer                           { \p _ -> Tok p TokNum                             }
  String                            { \p _ -> Tok p TokChars                             }
  Boolean                           { \p _ -> Tok p TokB                             }
  True                              { \p _ -> Tok p TokBT                              }
  False                             { \p _ -> Tok p TokBF                             }
  GraphNode                         { \p _ -> Tok p TokGrN                             }
  Rel                               { \p _ -> Tok p TokRType                             }
  ">="                              { \p _ -> Tok p TokEqualityG                               }
  "<="                              { \p _ -> Tok p TokEqualityL                               }
  \>                                { \p _ -> Tok p TokGT                                }
  \<                                { \p _ -> Tok p TokLT                                }
  \=                                { \p _ -> Tok p TokSet                            }
  "=="                              { \p _ -> Tok p TokEquals                            }
  \[                                { \p _ -> Tok p TokBracketLeftS                               }
  \]                                { \p _ -> Tok p TokBracketRightS                               }
  "<-"                              { \p _ -> Tok p TokLArrow                            }
  "->"                              { \p _ -> Tok p TokDirectedR                            }
  \-                                { \p _ -> Tok p TokHyph                              }
  r\".*\"                           { \p s -> Tok p (Tokrgx (drop 2 $ init s))           }
  \;                                { \p _ -> Tok p TokSCol                         }
  \!=                               { \p _ -> Tok p TokIneq                         }
  \,                                { \p _ -> Tok p TokSep                             }
  \.                                { \p _ -> Tok p TokBrk                               }
  \:[A-Z]+                          { \p s -> Tok p (TokFIdent (drop 1 s))               }
  \'($alpha|$digit|\.)*\'           { \p s -> Tok p (TokString (drop 1 $ init s))        }
  \"($alpha*)+\"                    { \p s -> Tok p (TokStringLiteral (drop 1 $ init s)) }
  \"                                { \p _ -> Tok p TokDelimiter                         }
  "||"                              { \p _ -> Tok p TokOr                                }
  "&&"                              { \p _ -> Tok p TokConj                               }
  \(                                { \p _ -> Tok p TokBracketLeft                            }
  \)                                { \p _ -> Tok p TokBracketRight                            }
  \{                                { \p _ -> Tok p TokLCurl                             }
  \}                                { \p _ -> Tok p TokRCurl                             }
  \:                                { \p _ -> Tok p TokNCol                             }
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
  | TokConj
  | TokBracketLeft
  | TokBracketRight
  | TokGr
  | TokNum
  | TokChars
  | TokStringLiteral String
  | TokB
  | TokLCurl
  | TokRCurl
  | TokCond
  | TokElse
  | TokFor
  | TokNCol
  | TokIdent String
  | TokEqualityG
  | TokEqualityL
  | TokGT
  | TokLT
  | TokSet
  | TokEquals
  | TokBracketLeftS
  | TokBracketRightS
  | TokLArrow
  | TokDirectedR
  | TokHyph
  | Tokrgx String
  | TokArith
  | TokBrk
  | TokFIdent String
  | TokString String
  | TokBT
  | TokBF
  | TokSCol
  | TokIneq
  | TokSep
  | TokInt Int
  | TokGrN
  | TokRType
  deriving (Eq, Show)

data Token = Tok AlexPosn TokenType
  deriving (Eq, Show)

tokenPosn :: Token -> String
tokenPosn (Tok (AlexPn _ line column) _) = show line ++ ":" ++ show column

lex :: String -> [Token]
lex str = alexScanTokens str
} 