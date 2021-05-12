module ProjectSpecificLabels

type ProjectSpecificLabel =
    | BinaryExpr
    | NumericExpr
    | StringLiteral
    | ColumnName of string
    | TableName of string

type ProjectSpecificError =
    | SQLASTError of SQLAST.ErrorMsg<DatabaseSchema.ProjectAppCol,DatabaseSchema.ProjectAppTable> list