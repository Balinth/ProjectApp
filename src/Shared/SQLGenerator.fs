module SQLGenerator

open System

open SQLAST
open DatabaseSchema
open ResultExtensions

type Parametrization = {
    ParamName : string * int
    ParamValue : Data
}

let getTables (db:DatabaseSchema<'c,'t>) cols =
    match cols with
    | [] -> Error [QueryHasNoColumns]
    | cols ->
        cols
        |> List.map db.GetTableNameByColumn
        |> List.distinct
        |> List.reduce (fun sum t -> sum + ", " + t)
        |> Ok

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

let stringizeBinaryNumericOperator op =
    match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"

let stringizeBoolOperator op =
    match op with
    | And -> "AND"
    | Or -> "OR"

let stringizeParameterName param =
    "@" + fst param.ParamName + (snd param.ParamName |> string)

let rec stringizeFieldExpr (db:DatabaseSchema<'c,'t>) ctx parameters expr =
    match expr with
    | Value v ->
        let param = parametrizeData ctx v
        (stringizeParameterName param,param::parameters)
    | Column c -> db.GetQualifiedColName c.Col, parameters
    | BracedFieldExpr(e) ->
        let innerExpr, innerParameters = stringizeFieldExpr db ctx parameters e
        (sprintf "(%s)" innerExpr, innerParameters)
    | BinaryFieldExpr(l, op, r) ->
        let leftExpr, leftParams = stringizeFieldExpr db ctx parameters l
        let rightExpr, rightParams = stringizeFieldExpr db ctx leftParams r
        (sprintf "%s %s %s" leftExpr (stringizeBinaryNumericOperator op) rightExpr, rightParams)

let stringizeRelationExpr db ctx parameters op e1 e2 : string*Parametrization list =
    let leftExpr, leftParameters = stringizeFieldExpr db ctx parameters e1
    let rightExpr, rightParameters = stringizeFieldExpr db ctx leftParameters e2
    (leftExpr + stringizeRelationOperator op + rightExpr, rightParameters)

let stringizeBoolLiteral = function | true -> "TRUE" | fales -> "FALSE"

let rec stringizeExpression (db: DatabaseSchema<'c,'t>) ctx parameters exp : string*Parametrization list =
    match exp with
    | RelationExpr (op,e1,e2) -> stringizeRelationExpr db ctx parameters e1 op e2
    | Not ex ->
        let expr, newParams = stringizeExpression db ctx parameters ex
        (sprintf "NOT %s" expr, newParams)
    | BinaryBoolExpr(l, op, r) ->
        let leftExpr, leftParams = stringizeExpression db ctx parameters l
        let rightExpr, rightParams = stringizeExpression db ctx leftParams r
        let op = stringizeBoolOperator op
        (sprintf "%s %s %s" leftExpr op rightExpr, rightParams)
    | BoolLiteral(lit) -> (stringizeBoolLiteral lit, parameters)
    | BracedBoolExpr(expr) ->
        let innerExpr, newParams = stringizeExpression db ctx parameters expr
        (sprintf "(%s)" innerExpr, newParams)

let stringizeSelect (db:DatabaseSchema<'c,'t>) (query:QueryStatement<'c>) =
    let cols = List.map (fun c -> c.Col) query.Columns
    let columnNames = cols |> List.map db.GetQualifiedColName
    getTables db cols
    >>= (fun tables ->
        "SELECT " + String.Join( ", ", columnNames) + " FROM " + tables
        |> Ok
    )

let map2 r1 r2 fn =
    match r1, r2 with
    | Ok o1, Ok o2 -> fn o1 o2 |> Ok
    | Error e1, Error e2 -> List.concat [e1; e2] |> Error
    | Error e1, _ -> Error e1
    | _, Error e2 -> Error e2

let stringizeSQLQuery db (query:QueryStatement<'c>)  =
    let ctx = ctxFactory()
    let select = stringizeSelect db query
    let where =
        match query.Condition with
        | Some cond ->
            stringizeExpression db ctx [] cond
            |> ((fun a -> "WHERE " + fst a, snd a) >> Ok)
        | None -> Ok ("", [])
    map2 select where (fun s w -> (s + Environment.NewLine + fst w), snd w)

let stringizeSQLInsert db statement =
    let cols =
        InsertStatement.cols statement

    let columns =
        cols
        |> List.map (fun cv -> db.GetColumnName cv.Column.Col)
        |> List.reduce (fun sum c -> sum + "," + c)
    let colParams =
        cols
        |> List.map (fun c -> "@"  + (db.GetColumnName c.Column.Col))
        |> List.reduce (fun sum c -> sum + "," + c)
    let sqlStr = "INSERT INTO " + (InsertStatement.table statement |> db.GetTableName) + " (" + columns + ") VALUES (" + colParams + ")"
    Ok (sqlStr, InsertStatement.cols statement)

(*
let testExpr = RelationExpr (Equals, (UserName |> UserTable |> ProjectAppColumns |> Column), (ProjectName |> ProjectTable |> ProjectAppColumns |>Column))
let testListExpr = ListExpr (And,[testExpr;testExpr])
let testConditions = ListExpr (And, [testListExpr;testExpr])
let testQuery = {Columns=[(UserName |> UserTable |> ProjectAppColumns )];Condition=Some testConditions}

let testSQL = stringizeSQLQuery projectAppDB testQuery
match testSQL with
| Ok str -> fst str |> printfn "%s"
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