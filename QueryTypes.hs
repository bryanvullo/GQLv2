module QueryTypes where

type File = [Query]

data Query = 
    SelectFile String |
    SelectQuery [Property] [Restriction]