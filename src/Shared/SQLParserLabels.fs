module ProjectSpecificLabels

type ProjectSpecificLabel =
    | BinaryExpr
    | NumericExpr
    | StringLiteral
    | ColumnName of string