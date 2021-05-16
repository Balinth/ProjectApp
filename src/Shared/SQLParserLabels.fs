module ProjectSpecificLabels

open SQLAST
open DatabaseSchema

type ProjectSpecificLabel =
    | SelectStatement
    | WhereExpression
    | BinaryExpr
    | NumericExpr
    | StringLiteral
    | DatabaseColumnName
    | DatabaseTableName
    | ColumnName of string
    | TableName of string

type ProjectSpecificError =
    | SQLASTError of ErrorMsg<ProjectAppCol,ProjectAppTable> list
    | SelectedColumnsTablesMismatch