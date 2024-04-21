module Syntax where

data Query
    = SelectQuery [PropertyRef] [NodePattern] (Maybe Condition)
    | CreateEdgeQuery NodePattern NodePattern EdgeType
    | CreateRelationshipQuery NodePattern NodePattern EdgeType
    | UpdateQuery [NodePattern] [PropertyUpdate]
    | DeleteQuery [NodePattern] (Maybe Condition)
    | DeleteEdgeQuery EdgeType
    deriving (Show)

data PropertyRef = PropertyRef String String
    deriving (Show)

data EdgePattern = EdgePattern NodePattern EdgeType NodePattern
    deriving (Show)

data NodePattern = NodePattern String
    deriving (Show)

data EdgeType = EdgeType String
    deriving (Show)

data Condition
    = ConditionAnd Condition Condition
    | ConditionOr Condition Condition
    | ConditionNot Condition
    | ConditionEqual PropertyRef Expression
    | ConditionLess PropertyRef Expression
    | ConditionGreater PropertyRef Expression
    | ConditionLessEqual PropertyRef Expression
    | ConditionGreaterEqual PropertyRef Expression
    | ConditionStartsWith PropertyRef Expression
    | ConditionHasLabel PropertyRef String
    | ConditionEdge NodePattern EdgePattern NodePattern
    deriving (Show)

data Expression
    = ExpressionInteger Integer
    | ExpressionString String
    | ExpressionBoolean Bool
    | ExpressionNull
    | ExpressionPropertyRef PropertyRef
    deriving (Show)

type PropertyUpdates = [PropertyUpdate]

data PropertyUpdate = PropertyUpdate PropertyRef Expression
    deriving (Show)