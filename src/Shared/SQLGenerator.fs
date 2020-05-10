module SQLGenerator

open System

open SQLAST
open DatabaseSchema

let getColumnType col =
    match col with
    | UserTable u ->
        match u with
        | UserName -> DBString
        | PrimaryEmail -> DBString
        | UserNameID -> DBString
        | UserID -> DBInt
    | ProjectTable p ->
        match p with
        | ProjectName -> DBString
        | ProjectID -> DBString
        | StartDate -> DBInt
        | Code -> DBInt

let ProjectAppColumns col = { Col = col; Type = getColumnType col }

let qualifiedColName col = getColumnTableName col + "." + getColumnName col

let getTables cols =
    match cols with
    | [] -> Error [QueryHasNoColumns]
    | cols ->
        cols
        |> List.map getColumnTableName
        |> List.distinct
        |> List.reduce (fun sum t -> sum + ", " + t)
        |> Ok

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
    | Column c -> qualifiedColName c.Col, None

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

let rec stringizeListExpr ctx op (exprs : Expression<'c> list) : Result<string*Parametrization list,ErrorMsg list> =
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

let stringizeSelect query =
    let cols = List.map (fun c -> c.Col) query.Columns
    let columnNames = cols |> List.map qualifiedColName
    getTables cols
    >>= (fun tables ->
        "SELECT " + String.Join( ", ", columnNames) + " FROM " + tables
        |> Ok
    )

let map2 r1 r2 fn =
    match r1, r2 with
    | Ok o1, Ok o2 -> fn o1 o2 |> Ok
    | Error e1, Error e2 -> List.append e1 e2 |> Error
    | Error e1, _ -> Error e1
    | _, Error e2 -> Error e2

let stringizeSQLQuery query =
    let ctx = ctxFactory()
    let select = stringizeSelect query
    let where =
        match query.Condition with
        | Some cond ->
            stringizeExpression ctx cond
            >>= ((fun a -> "WHERE " + fst a, snd a) >> Ok)
        | None -> Ok ("", [])
    map2 select where (fun s w -> (s + Environment.NewLine + fst w), snd w)

type InsertValue<'c> = {
    Column : Column<'c>
    Value : Data
}

type InsertStatement<'c> = {
    Columns : InsertValue<'c> list
}

let getInsertTable (statement : InsertStatement<'c>) =
    let tables = List.map ((fun c -> c.Column.Col) >> getColumnTableName) statement.Columns |> List.distinct
    match tables with
    | [] -> Error [InsertMustHaveColumns]
    | [table] -> Ok table
    | list -> Error [InsertMustTargetOneTable list]

let ensureDistinctColumns (statement : InsertStatement<'c>) =
    let cols = statement.Columns |> List.map (fun c -> c.Column.Col |> getColumnName)
    match cols with
    | [] -> Error InsertMustHaveColumns
    | list when list.Length = (List.distinct cols).Length -> Ok list
    | _ -> InsertMustContainDistinctColumns (cols) |> Error

let createInsertSQL statement table =
    statement
    |> ensureDistinctColumns
    |> function
        | Ok cols ->
            let columns =
                cols
                |> List.reduce (fun sum c -> sum + "," + c)
            let colParams =
                cols
                |> List.map (fun c -> "@" + c)
                |> List.reduce (fun sum c -> sum + "," + c)
            let sqlStr = "INSERT INTO " + table + "(" + columns + ") VALUES (" + colParams + ")"
            Ok (sqlStr, cols)
        | Error err -> Error [err]


let stringizeSQLInsert (statement : InsertStatement<'c>) =
    getInsertTable statement
    >>= createInsertSQL statement


let testExpr = RelationExpr (Equals, (UserName |> UserTable |> ProjectAppColumns |> Column), (ProjectName |> ProjectTable |> ProjectAppColumns |>Column))
let testListExpr = ListExpr (And,[testExpr;testExpr])
let testConditions = ListExpr (And, [testListExpr;testExpr])
let testQuery = {Columns=[(UserName |> UserTable |> ProjectAppColumns )];Condition=Some testConditions}

let testSQL = stringizeSQLQuery testQuery
match testSQL with
| Ok str -> fst str |> printfn "%s"
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