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

type RelationOp =
    | Equals
    | Greater
    | GreaterOrEquals
    | Smaller
    | SmallerOrEquals
    | NotEquals

type BinaryBooleanOp =
    | And
    | Or

type BinaryNumericOp =
    | Add
    | Sub
    | Mul
    | Div

type UnaryBinaryOperator = | Not

type FieldExpr<'c> =
    | Value of Data
    | Column of Column<'c>
    | BracedFieldExpr of FieldExpr<'c>
    | BinaryFieldExpr of FieldExpr<'c> * BinaryNumericOp * FieldExpr<'c>

type BoolExpr<'c> =
    | BinaryBoolExpr of BoolExpr<'c> * BinaryBooleanOp * BoolExpr<'c>
    | RelationExpr of FieldExpr<'c> * RelationOp * FieldExpr<'c>
    | Not of BoolExpr<'c>
    | BoolLiteral of bool
    | BracedBoolExpr of BoolExpr<'c>

type ErrorMsg<'c> =
    | QueryHasNoColumns
    | SyntaxError
    | OperatorMustHaveArguments of BinaryBooleanOp
    | InsertMustHaveColumns
    | InsertMustTargetOneTable of string list
    | InsertMustContainDistinctColumns of 'c list

 
type QueryStatement<'c> = {
    Columns : Column<'c> list
    Condition : BoolExpr<'c> option
}

type InsertValue<'c> = {
    Column : Column<'c>
    Value : Data
}

type InsertStatement<'c> = {
    Columns : InsertValue<'c> list
}
