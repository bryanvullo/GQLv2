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

%%
Tables : Table                                          { [$1] }
       | Table Tables                                   { $1 : $2 }

Table : Header Rows                                     { $1 : $2 }
                
Header : ':' id ',' Types                                  { Header $4 }
       | ':' id ',' Types ',' ':' label                    { LabeledHeader $4 }
       | ':' startId ',' Types ',' ':' endId ',' ':' type  { RelationshipHeader $4 }

Types : Type                                            { [$1] }
      | Type ',' Types                                  { $1 : $3 }

Type : string ':' stringType                            { StringType $1 }
     | alphanum ':' stringType                          { StringType $1 }
     | string ':' intType                               { IntType $1 }
     | alphanum ':' intType                             { IntType $1 }
     | string ':' boolType                              { BoolType $1 }
     | alphanum ':' boolType                            { BoolType $1 }      

Rows : Row                                              { [$1] }
     | Row Rows                                         { $1 : $2 }

Row : ID ',' Values                                     { Data $1 $3 }
    | ID ',' Values ',' Labels                          { LabeledData $1 $3 $5 }
    | StartID ',' Values ',' EndID ',' Relationship     { RelationshipData $1 $3 $5 $7 }

ID : string                                             { Id $1 }
   | alphanum                                           { Id $1 }

StartID : string                                        { StartID $1 }
        | alphanum                                      { StartID $1 }

EndID : string                                          { EndID $1 }
      | alphanum                                        { EndID $1 }

Labels : string                                         { [$1] }
       | string ';' Labels                              { $1 : $3 }

Relationship : string                                   { $1 }

Values : Value                                          { [$1] }
       | Value ',' Values                               { $1 : $3 }

Value : '"' string '"'                                  { StringValue $2 }
      | int                                             { IntValue $1 }
      | bool                                            { BoolValue $1 }
      | null                                            { NullValue }

{

parseError :: [Token] -> a
parseError tokens@(x:xs) = error $ "Parse error: " ++ tokenPosn (head tokens) ++ "\n" ++ show tokens
parseError [] = error "Parse error: end of input"

type Tables = [Table]

type Table = [Row]

type Types = [Type]

type Values = [Value]

data ID = Id String 
        | StartID String 
        | EndID String
        deriving (Eq,Show)

type Relationship = String

type Labels = [String]

data Row = 
    Header Types |
    LabeledHeader Types |
    RelationshipHeader Types |
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
    Labels [String] |
    StringValue String |
    IntValue Int |
    BoolValue Bool
    deriving (Eq,Show)

}