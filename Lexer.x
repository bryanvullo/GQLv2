{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  ACCESS                              { \p _ -> Tok p TokACCESS                              }
  FIND                              { \p _ -> Tok p TokFIND                              }
  OUT                               { \p _ -> Tok p TokOUT                               }
  CONDITION                                { \p _ -> Tok p TokCond                              }
  ELSE                              { \p _ -> Tok p TokCondE                              }
  LOOPF                               { \p _ -> Tok p TokLoopF                               }
  ARITH                               { \p _ -> Tok p TokArith                               }
  Gr                             { \p _ -> Tok p TokGr                             }
  Integer                           { \p _ -> Tok p TokNum                             }
  String                            { \p _ -> Tok p TokChars                             }
  Boolean                           { \p _ -> Tok p TokB                             }
  True                              { \p _ -> Tok p TokBT                              }
  False                             { \p _ -> Tok p TokBF                             }
  GrNode                         { \p _ -> Tok p TokGrN                             }
  Rel                               { \p _ -> Tok p TokRel                             }
  ">="                              { \p _ -> Tok p TokEqualityEqG                               }
  "<="                              { \p _ -> Tok p TokEqualityEqL                               }
  \>                                { \p _ -> Tok p TokEqualityG                                }
  \<                                { \p _ -> Tok p TokEqualityL                                }
  \=                                { \p _ -> Tok p TokSet                            }
  "=="                              { \p _ -> Tok p TokExact                            }
  \[                                { \p _ -> Tok p TokBracketLeftS                               }
  \]                                { \p _ -> Tok p TokBracketRightS                               }
  "<-"                              { \p _ -> Tok p TokDirectedL                            }
  "->"                              { \p _ -> Tok p TokDirectedR                            }
  \-                                { \p _ -> Tok p TokHyph                              }
  r\".*\"                           { \p s -> Tok p (Tokrgx (drop 2 $ init s))           }
  \;                                { \p _ -> Tok p TokColS                         }
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
  \{                                { \p _ -> Tok p TokBracketLeftC                             }
  \}                                { \p _ -> Tok p TokBracketRightC                             }
  \:                                { \p _ -> Tok p TokColN                             }
  [a-z]($alpha|$digit)*             { \p s -> Tok p (TokIdent s)                         }
  $digit+                           { \p s -> Tok p (TokInt (read s :: Int))             }
  $white+                           ;
  "--".*                            ;

{
data TokenType
  = TokACCESS
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
  | TokBracketLeftC
  | TokBracketRightC
  | TokCond
  | TokCondE
  | TokLoopF
  | TokColN
  | TokIdent String
  | TokEqualityEqG
  | TokEqualityEqL
  | TokEqualityG
  | TokEqualityL
  | TokSet
  | TokExact
  | TokBracketLeftS
  | TokBracketRightS
  | TokDirectedL
  | TokDirectedR
  | TokHyph
  | Tokrgx String
  | TokArith
  | TokBrk
  | TokFIdent String
  | TokString String
  | TokBT
  | TokBF
  | TokColS
  | TokIneq
  | TokSep
  | TokInt Int
  | TokGrN
  | TokRel
  deriving (Eq, Show)

data Token = Tok AlexPosn TokenType
  deriving (Eq, Show)

tokenPosn :: Token -> String
tokenPosn (Tok (AlexPn _ line column) _) = show line ++ ":" ++ show column

lex :: String -> [Token]
lex str = alexScanTokens str
} 