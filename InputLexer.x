{
module InputLexer (alexScanTokens, Token(..), TokenType(..), lexInput, tokenPosn) where
}

%wrapper "posn"

$digit = 0-9
$lower = [a-z]
$upper = [A-Z]
$alpha = [$lower $upper]
$alphanum = [$alpha $digit]

tokens :-
  "string"                          { \p _ -> Tok p TokStringType }
  "integer"                         { \p _ -> Tok p TokIntType }
  "boolean"                         { \p _ -> Tok p TokBoolType }
  "null"                            { \p _ -> Tok p TokNull }

  "ID"                              { \p _ -> Tok p TokId }     
  "START_ID"                        { \p _ -> Tok p TokStartId }      
  "END_ID"                          { \p _ -> Tok p TokEndId }        
  "TYPE"                            { \p _ -> Tok p TokType }   
  "LABEL"                           { \p _ -> Tok p TokLabel }

  [\+ \-]? $digit+                  { \p s -> Tok p (TokInt (read s)) }
  "false"                           { \p _ -> Tok p (TokBool False) }
  "true"                            { \p _ -> Tok p (TokBool True) }
  $alpha+                           { \p s -> Tok p (TokString s) }
  $alphanum+                        { \p s -> Tok p (TokAlphaNum s) }

  \:                                { \p _ -> Tok p TokColon }
  \;                                { \p _ -> Tok p TokSemiColon }        
  \,                                { \p _ -> Tok p TokComma }
  \"                                { \p _ -> Tok p TokQuoteMark }

  $white+                           ;
  "--".*                            ;
  
{

data TokenType
  = TokColon --punctuation
  | TokSemiColon
  | TokComma
  | TokQuoteMark
  | TokNewLine
  | TokId --keywords
  | TokStartId
  | TokEndId
  | TokType
  | TokLabel
  | TokBoolType --types
  | TokIntType
  | TokStringType
  | TokString String --values
  | TokAlphaNum String
  | TokBool Bool
  | TokInt Int
  | TokNull
  deriving (Eq, Show)

data Token = Tok AlexPosn TokenType
  deriving (Eq, Show)

tokenPosn :: Token -> String
tokenPosn (Tok (AlexPn _ line column) _) = show line ++ ":" ++ show column

lexInput :: String -> [Token]
lexInput str = alexScanTokens str
}