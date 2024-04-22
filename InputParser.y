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

%left ',' ':' ';'

%%
Tables : {- empty -}                                    { [] }
       | Table Tables                                   { $1 : $2 }

Table : Header Rows                                     { $1 : $2 }
                
Header : id Types                                       { Header $2 }
       | id Types ',' label                             { LabeledHeader $2 }
       | startId Types ',' endId ',' type               { RelationshipHeader $2 }

Types : {- empty -}                                     { [] }
      | ',' Type Types                                  { $2 : $3 }

Type : Name ':' stringType                              { StringType $1 }
     | Name ':' intType                                 { IntType $1 }
     | Name ':' boolType                                { BoolType $1 }  

Name : string                                           { $1 }
     | alphanum                                         { $1 }    

Rows : {- empty -}                                      { [] }
     | Row Rows                                         { $1 : $2 }

Row : alphanum Values                                   { Data (Id $1) $2 }
    | alphanum Values ',' Labels                        { LabeledData (Id $1) $2 $4 }
    | alphanum Values ',' alphanum ',' string           { RelationshipData (Id $1) $2 (Id $4) $6 }

Labels : {- empty -}                                    { [] }
       | string                                         { [$1] }
       | string ';' Labels                              { $1 : $3 }

Values : {- empty -}                                    { [] }     
       | Value Values                                   { $1 : $2 }

Value : ',' '"' string '"'                              { StringValue $3 }
      | ',' int                                         { IntValue $2 }
      | ',' bool                                        { BoolValue $2 }
      | ',' null                                        { NullValue }

{

parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ tokenPosn (head tokens) ++ "\n" ++ show tokens

type Tables = [Table]

type Table = [Row]

-- data Table = Table Header [Row]
--     deriving (Eq,Show)

type Types = [Type]

data ID = Id String 
        deriving (Eq,Show)

type Relationship = String

type Labels = [String]

-- data Header = 
--     Header Types |
--     LabeledHeader Types |
--     RelationshipHeader Types
--     deriving (Eq,Show)

data Row = 
    Header Types |
    LabeledHeader Types |
    RelationshipHeader Types |
    Data ID [Value] |
    LabeledData ID [Value] Labels |
    RelationshipData ID [Value] ID Relationship
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