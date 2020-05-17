module SQLAST

type DBType =
    | DBString
    | DBInt
    | DBFloat

type Data =
    | String of string
    | Int of int
    | Float of float

type Column<'c> = {
    Col : 'c
    Type : DBType
}

type RelationOperator =
    | Equals
    | Greater
    | GreaterOrEquals
    | Smaller
    | SmallerOrEquals
    | NotEquals

type BooleanOperator =
    | And
    | Or

type Operator =
    | BooleanOperator of BooleanOperator
    | RelationOperator of RelationOperator

type FieldExpression<'c> =
    | Value of Data
    | Column of Column<'c>

type Expression<'c> =
    | ListExpr of BooleanOperator * Expression<'c> list
    | RelationExpr of RelationOperator * FieldExpression<'c> * FieldExpression<'c>
    | Not of Expression<'c>

type ErrorMsg<'c> =
    | QueryHasNoColumns
    | SyntaxError
    | OperatorMustHaveArguments of BooleanOperator
    | InsertMustHaveColumns
    | InsertMustTargetOneTable of string list
    | InsertMustContainDistinctColumns of 'c list

 
type QueryStatement<'c> = {
    Columns : Column<'c> list
    Condition : Expression<'c> option
}

type InsertValue<'c> = {
    Column : Column<'c>
    Value : Data
}

type InsertStatement<'c> = {
    Columns : InsertValue<'c> list
}
