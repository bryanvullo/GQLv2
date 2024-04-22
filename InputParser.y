{
module InputParser where
import InputLexer (Token(..), TokenType(..), tokenPosn)
}

%name parseInput
%tokentype { Token }
%error { parseError }

%token
    stringType          { Tok _ TokStringType }  
    intType             { Tok _ TokIntType }
    boolType            { Tok _ TokBoolType }
    id                  { Tok _ TokId }      
    startId             { Tok _ TokStartId }
    endId               { Tok _ TokEndId }
    label               { Tok _ TokLabel }
    type                { Tok _ TokType }
    string              { Tok _ (TokString $$) }
    alphanum            { Tok _ (TokAlphaNum $$) }
    int                 { Tok _ (TokInt $$) }
    bool                { Tok _ (TokBool $$) }
    null                { Tok _ TokNull }
    ':'                 { Tok _ TokColon }
    ';'                 { Tok _ TokSemiColon }
    ','                 { Tok _ TokComma }
    '"'                 { Tok _ TokQuoteMark }
    '\n'                  { Tok _ TokNL }

%left ','

%%
Tables : {- empty -}                                    { [] }
       | Table NewLines Tables                          { $1 : $3 }

Table : Header '\n' Rows                                { Table $1 $3 }
                
Header : ':' id Types                                   { Header $3 }
       | ':' id Types ',' ':' label                     { LabeledHeader $3 }
       | ':' startId Types ',' ':' endId ',' ':' type   { RelationshipHeader $3 }

Types : {- empty -}                                     { [] }
      | ',' Type Types                                  { $2 : $3 }

Type : String ':' stringType                            { StringType $1 }
     | String ':' intType                               { IntType $1 }
     | String ':' boolType                              { BoolType $1 }  

String : string                                         { $1 }
       | alphanum                                       { $1 }    

Rows : {- empty -}                                      { [] }
     | Row Rows                                         { $1 : $2 }

Row : ID Values '\n'                                    { Data $1 $2 }
    | ID Values ',' Labels '\n'                         { LabeledData $1 $2 $4 }
    | StartID Values ',' EndID ',' Relationship '\n'    { RelationshipData $1 $2 $4 $6 }

ID : string                                             { Id $1 }
   | alphanum                                           { Id $1 }

StartID : string                                        { StartID $1 }
        | alphanum                                      { StartID $1 }

EndID : string                                          { EndID $1 }
      | alphanum                                        { EndID $1 }

Labels : {- empty -}                                    { [] }
       | string                                         { [$1] }
       | string ';' Labels                              { $1 : $3 }

Relationship : string                                   { $1 }

Values : {- empty -}                                    { [] }
    --    | ',' Value                                      { [ $2 ] }
       | ',' Value Values                               { $2 : $3 }

Value : '"' string '"'                                  { StringValue $2 }
      | int                                             { IntValue $1 }
      | bool                                            { BoolValue $1 }
      | null                                            { NullValue }

NewLines : {- empty -}                                  {}
         | '\n' NewLines                                  {}

{

parseError :: [Token] -> a
parseError tokens@(x:xs) = error $ "Parse error: " ++ tokenPosn (head tokens) ++ "\n" ++ show tokens
parseError [] = error "Parse error: end of input"

type Tables = [Table]

data Table = Table Header [Row]
    deriving (Eq,Show)

type Types = [Type]

type Values = [Value]

data ID = Id String 
        | StartID String 
        | EndID String
        deriving (Eq,Show)

type Relationship = String

type Labels = [String]

data Header = 
    Header Types |
    LabeledHeader Types |
    RelationshipHeader Types
    deriving (Eq,Show)

data Row = 
    Data ID Values |
    LabeledData ID Values Labels |
    RelationshipData ID Values ID Relationship
    deriving (Eq,Show)

data Type = 
    StringType String |
    IntType String |
    BoolType String
    deriving (Eq,Show)

data Value =
    NullValue |
    StringValue String |
    IntValue Int |
    BoolValue Bool
    deriving (Eq,Show)

}