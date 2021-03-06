module SQLAST

open ResultExtensions

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

type ErrorMsg<'column,'table> =
    | QueryHasNoColumns
    | SyntaxError
    | OperatorMustHaveArguments of BinaryBooleanOp
    | AllColumnsMustBeFromTable of expectedTable: 'table * externalColumns: 'column list
    | InsertMustHaveColumns
    | InsertMustContainDistinctColumns of nonDistinct: 'column list
    | InsertColumnAndDataCountMismatch of columns: 'column list * data: Data list

type DatabaseSchema<'column,'table> = {
    GetColumnTable: 'column -> 'table
    GetTableName: 'table -> string
    GetColumnName: 'column -> string
    GetColumnType: 'column -> DBType
    }
    with
        member this.GetColumn colCase =
            {Col=colCase;Type=this.GetColumnType colCase}
        member this.GetTableNameByColumn colCase =
            colCase |> this.GetColumnTable |> this.GetTableName
        member this.GetQualifiedColName colCase =
            this.GetTableNameByColumn colCase + "." + (this.GetColumnName colCase)

type QueryStatement<'c> = {
    Columns : Column<'c> list
    Condition : BoolExpr<'c> option
}

type InsertValue<'c> = {
    Column : Column<'c>
    Value : Data
}

type InsertStatement<'c,'t> = private {
    Table: 't
    Columns : InsertValue<'c> list
}

// utility functions

let ensureMoreThanOneAndUniqueColumns cols =
    match List.distinct cols with
    | [] -> Error [InsertMustHaveColumns]
    | distinctCols when distinctCols.Length <> cols.Length ->
        let filter = fun c -> Set.ofList distinctCols |> Set.contains c
        cols
        |> List.filter filter
        |> InsertMustContainDistinctColumns
        |> List.singleton
        |> Error
    | distinctCols -> Ok distinctCols

module InsertStatement =
    let create dbSchema table columns (datas: Data list ) =
        let distinctCols = ensureMoreThanOneAndUniqueColumns columns
        let columnsFromWrongTable =
            columns
            |> List.filter (fun c -> dbSchema.GetColumnTable c <> table)
            |> function
                | [] -> Ok ()
                | externalColumns -> [AllColumnsMustBeFromTable (table, externalColumns)] |> Error

        let columnsAndDatas =
            match columns, datas with
            | (columns, datas) when columns.Length = datas.Length ->
                List.zip columns datas |> Ok
            | _ -> [InsertColumnAndDataCountMismatch(columns, datas)] |> Error

        let createFromVerified _ columnsAndDatas _ =
            columnsAndDatas
            |> List.map (fun (c, d) -> {Column=dbSchema.GetColumn c;Value=d})
            |> fun inserts -> {Columns=inserts;Table=table}

        createFromVerified
        <!> distinctCols
        <*> columnsAndDatas
        <*> columnsFromWrongTable

        //match columnsFromWrongTable, distinctCols, columnsAndDatas with
        //// the only OK case...
        //| [], Ok _, Ok columnsAndDatas ->
        //    columnsAndDatas
        //    |> List.map (fun (c, d) -> {Column=dbSchema.GetColumn c;Value=d})
        //    |> fun inserts -> {Columns=inserts;Table=table}
        //    |> Ok
        //// all the ways this can go wrong
        //| [], Error distinctError, Error colDataError -> Error [distinctError; colDataError]
        //| externalColumns, Error e ->
        //    [AllColumnsMustBeFromTable (table, externalColumns);e] |> Error
        //| externalColumns, Ok _ -> [AllColumnsMustBeFromTable (table, externalColumns)] |> Error
    let cols statement = statement.Columns
    let table statement = statement.Table