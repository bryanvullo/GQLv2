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
    '\n'                { Tok _ TokNewLine }

%left ':' ';' label
%right ','

%%
Tables : NL                                    { [] }
       | Table NL Tables                                   { $1 : $3 }

Table : Header Rows                                     { $1 : $2 }
                
Header : id Types NL                                   { Header $2 }
       | startId Types endId ',' type NL           { RelationshipHeader $2 }
       | id Types label NL                         { LabeledHeader $2 }

Types : {- empty -}                                            { [ ] }
      | ','                                         { [ ] }
      | Type Types                                  { $1 : $2 }

Type : ',' Name ':' stringType                              { StringType $2 }
     | ',' Name ':' intType                                 { IntType $2 }
     | ',' Name ':' boolType                                { BoolType $2 }  

Name : string                                           { $1 }
     | alphanum                                         { $1 }    

Rows : RowNoNL                                      { [$1] }
     | Row                                              { [$1] }
     | Row Rows                                         { $1 : $2 }

Row : alphanum Values NL                                   { Data (Id $1) $2 }
    | alphanum Values Labels NL                        { LabeledData (Id $1) $2 $3 }
    | alphanum Values alphanum ',' string NL           { RelationshipData (Id $1) $2 (Id $3) $5 }

RowNoNL : alphanum Values                                    { Data (Id $1) $2 }
        | alphanum Values Labels                        { LabeledData (Id $1) $2 $3 }
        | alphanum Values alphanum ',' string           { RelationshipData (Id $1) $2 (Id $3) $5 }

Labels : string                                         { [$1] }
       | string ';' Labels                              { $1 : $3 }

Values : {- empty -}                                    { [] }  
       | ','                                            { [] }
       | Value Values                                   { $1 : $2 }

Value : ',' '"' string '"'                              { StringValue $3 }
      | ',' int                                         { IntValue $2 }
      | ',' bool                                        { BoolValue $2 }
      | ',' null                                        { NullValue }

NL : {- empty -}                                              { }
   | '\n' NL                                           { }

{

parseError :: [Token] -> a
parseError [] = error "Parse error at end of input"
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