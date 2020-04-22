module SQLAST

open System

type DBType =
    | DBString
    | DBInt
    | DBFloat

type Column = {
    Table : string
    Name : string
    DataType : DBType
}

type Data =
    | String of string
    | Int of int
    | Float of float

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

type FieldExpression =
    | Value of Data
    | Column of Column

type Expression =
    | ListExpr of BooleanOperator * Expression list
    | RelationExpr of RelationOperator * FieldExpression * FieldExpression
    | Not of Expression

type ErrorMsg =
    | SyntaxError
    | OperatorMustHaveArguments of BooleanOperator
    | InsertMustHaveColumns
    | InsertMustTargetOneTable of string list
    | InsertMustContainDistinctColumns of string list

type Parametrization = {
    ParamName : string * int
    ParamValue : Data
}

let (>>=) x y = Result.bind y x

let ctxFactory ()=
    let innerFn =
        let mutable counter = 0
        fun () ->
            counter <- counter + 1
            counter
    innerFn

let paramStr = "Param"

let parametrizeData ctx data =
    {ParamName=(paramStr,ctx());ParamValue=data}

let stringizeRelationOperator op =
    match op with
    | Equals -> "="
    | NotEquals -> "<>"
    | Greater -> ">"
    | GreaterOrEquals -> ">="
    | Smaller -> "<"
    | SmallerOrEquals -> "<="

let stringizeBoolOperator op =
    match op with
    | And -> "AND"
    | Or -> "OR"

let stringizeFieldExpr ctx expr =
    match expr with
    | Value v ->
        let param = parametrizeData ctx v
        ("@" + fst param.ParamName + (snd param.ParamName |> string),Some param)
    | Column c -> c.Table + "." + c.Name, None

let stringizeRelationExpr ctx op e1 e2 : Result<string*Parametrization list,ErrorMsg list> =
    let fieldStr1, parameter1 = stringizeFieldExpr ctx e1
    let fieldStr2, parameter2 = stringizeFieldExpr ctx e2
    let parameters = [parameter1; parameter2] |> List.collect Option.toList
    (fieldStr1 + stringizeRelationOperator op + fieldStr2, parameters)
    |> Ok

let resultApply f (a : Result<'a,ErrorMsg list>) (b : Result<'a,ErrorMsg list>)=
    match a with
    | Error aErr ->
        match b with
        | Error bErr -> List.concat [aErr;bErr] |> Error
        | Ok bOk -> Error aErr
    | Ok aOk ->
        match b with
        | Error bErr -> Error bErr
        | Ok bOk -> f aOk bOk


let wrapExp exp =
    "(" + exp + ")"

let rec stringizeListExpr ctx op (exprs : Expression list) : Result<string*Parametrization list,ErrorMsg list> =
    let opStr = stringizeBoolOperator op
    if exprs.IsEmpty then Error [(OperatorMustHaveArguments op)]
    else
        exprs 
        |> List.map (stringizeExpression ctx)
        |> List.fold ((resultApply (fun a b-> 
            let strPart = fst a + (if (fst a).Length <> 0 then " " + opStr + " " else "") + (fst b)
            let parametersPart = List.concat [snd a; (snd b)]
            Ok(strPart,parametersPart)
            ))) (Ok ("",[]))

and stringizeExpression ctx exp : Result<string*Parametrization list,ErrorMsg list> =
    match exp with
    | ListExpr (op,exps) -> stringizeListExpr ctx op exps
    | RelationExpr (op,e1,e2) -> stringizeRelationExpr ctx op e1 e2
    | Not ex -> stringizeExpression ctx ex
    |> Result.map (fun (s,p) -> (wrapExp s), p)

type Query = {
    Table : string
    Columns : Column list
    Condition : Expression option
}

let stringizeSQLQuery query =
    let ctx = ctxFactory()
    let columns = query.Columns |> List.map (fun c -> c.Table + "." + c.Name)
    let select = "SELECT " + String.Join( ", ", columns) + " FROM " + query.Table
    match query.Condition with
    | Some cond ->
        stringizeExpression ctx cond
        >>= ((fun a -> "WHERE " + fst a, snd a) >> Ok)
    | None -> Ok ("", [])
    >>= (fun where -> ((select + Environment.NewLine + fst where), snd where) |> Ok)

type InsertValue = {
    Column : Column
    Value : Data
}

type InsertStatement = {
    Columns : InsertValue list
}

let getInsertTable (statement : InsertStatement) =
    let tables = statement.Columns |> List.map (fun c -> c.Column.Table) |> List.distinct
    match tables with
    | [] -> Error [InsertMustHaveColumns]
    | [table] -> Ok table
    | list -> Error [InsertMustTargetOneTable list]

let ensureDistinctColumns (statement : InsertStatement) =
    let distinctCols = List.distinctBy (fun c -> c.Column.Name) statement.Columns
    match distinctCols with
    | [] -> Error InsertMustHaveColumns
    | list when list = statement.Columns -> Ok list
    | _ -> InsertMustContainDistinctColumns (statement.Columns |> List.map (fun c -> c.Column.Name)) |> Error

let createInsertSQL statement table =
    statement
    |> ensureDistinctColumns
    |> function
        | Ok cols ->
            let columns =
                cols
                |> List.map (fun c -> c.Column.Name)
                |> List.reduce (fun sum c -> sum + "," + c)
            let colParams =
                cols
                |> List.map (fun c -> "@" + c.Column.Name)
                |> List.reduce (fun sum c -> sum + "," + c)
            let sqlStr = "INSERT INTO " + table + "(" + columns + ") VALUES (" + colParams + ")"
            Ok (sqlStr, cols)
        | Error err -> Error [err]


let stringizeSQLInsert (statement : InsertStatement) =
    getInsertTable statement
    >>= createInsertSQL statement



(*
let testCols = [
    {Table="T1";Name="ID";DataType=DBString}
    {Table="T1";Name="Name";DataType=DBString}
    {Table="T1";Name="Mass";DataType="int"}
    ]

let testExpr = RelationExpr (Equals, (Column testCols.[0]), (Column testCols.[1]))
let testListExpr = ListExpr (And,[testExpr;testExpr])
let testConditions = ListExpr (And, [testListExpr;testExpr])
let testQuery = {Table="User"; Columns=testCols;Condition=Some testConditions}
*)
(*

let UserColumns = 
    [
        ("UserName", {Table="User";Name="UserName";DataType=DBString})
        ("UserNameID",{Table="User";Name="UserNameID";DataType=DBString})
        ("PrimaryEmail",{Table="User";Name="PrimaryEmail";DataType=DBString})
    ]
    |> Map.ofSeq
let testExpr = RelationExpr (Equals ,Column UserColumns.["UserName"],"test" |> String |> Value)
let testQuery = {Table="User";Columns=UserColumns |> List.ofSeq |> List.map (fun i -> i.Value);Condition = Some testExpr}
let testStr = stringizeSQL testQuery
testStr

match testStr with
| Ok (s,p) -> s
| _ -> "fos"
*)