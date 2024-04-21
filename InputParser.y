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

%left ','

%%
Tables : {- empty -}                                    { [] }
       | Tables Table                                   { $2 : $1 }

Table : Header Rows                                     { $1 : $2 }
                
Header : ':' id Types                                   { Header $3 }
       | ':' id Types ',' ':' label                     { LabeledHeader $3 }
       | ':' startId Types ',' ':' endId ',' ':' type   { RelationshipHeader $3 }

Types : {- empty -}                                     { [] }
      | Types ',' Type                                  { $3 : $1 }

Type : string ':' stringType                            { StringType $1 }
     | alphanum ':' stringType                          { StringType $1 }
     | string ':' intType                               { IntType $1 }
     | alphanum ':' intType                             { IntType $1 }
     | string ':' boolType                              { BoolType $1 }
     | alphanum ':' boolType                            { BoolType $1 }      

Rows : {- empty -}                                      { [] }
     | Rows Row                                         { $2 : $1 }

Row : ID Values                                         { Data $1 $2 }
    | ID Values ',' Labels                              { LabeledData $1 $2 $4 }
    | StartID Values ',' EndID ',' Relationship         { RelationshipData $1 $2 $4 $6 }

ID : string                                             { Id $1 }
   | alphanum                                           { Id $1 }

StartID : string                                        { StartID $1 }
        | alphanum                                      { StartID $1 }

EndID : string                                          { EndID $1 }
      | alphanum                                        { EndID $1 }

Labels : string                                         { [$1] }
       | Labels ';' string                              { $3 : $1 }

Relationship : string                                   { $1 }

Values : {- empty -}                                    { [] }
       | Values ',' Value                               { $3 : $1 }

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
    StringValue String |
    IntValue Int |
    BoolValue Bool
    deriving (Eq,Show)

}