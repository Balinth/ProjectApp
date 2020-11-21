module SQLGenerator

open System

open SQLAST
open DatabaseSchema
open ResultExtensions

type DB<'c> = {
    ColumnName : 'c -> string
    ColumnTableName : 'c -> string
    ColumnType : 'c -> DBType
    QualifiedColName : 'c -> string
}

type Parametrization = {
    ParamName : string * int
    ParamValue : Data
}

let getColumnType col =
    match col with
    | UserTable u ->
        match u with
        | UserName -> DBString
        | PrimaryEmail -> DBString
        | UserNameID -> DBString
        | UserID -> DBInt
        | GivenName -> DBString
        | FamilyName -> DBString
    | ProjectTable p ->
        match p with
        | ProjectName -> DBString
        | ProjectID -> DBString
        | StartDate -> DBInt
        | Code -> DBInt

let getTables db cols =
    match cols with
    | [] -> Error [QueryHasNoColumns]
    | cols ->
        cols
        |> List.map db.ColumnTableName
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

let stringizeBoolOperator op =
    match op with
    | And -> "AND"
    | Or -> "OR"

let stringizeFieldExpr db ctx expr =
    match expr with
    | Value v ->
        let param = parametrizeData ctx v
        ("@" + fst param.ParamName + (snd param.ParamName |> string),Some param)
    | Column c -> db.QualifiedColName c.Col, None

let stringizeRelationExpr db ctx op e1 e2 : Result<string*Parametrization list,ErrorMsg<'c> list> =
    let fieldStr1, parameter1 = stringizeFieldExpr db ctx e1
    let fieldStr2, parameter2 = stringizeFieldExpr db ctx e2
    let parameters = [parameter1; parameter2] |> List.collect Option.toList
    (fieldStr1 + stringizeRelationOperator op + fieldStr2, parameters)
    |> Ok

let wrapExp exp =
    "(" + exp + ")"

let rec stringizeListExpr db ctx op (exprs : BoolExpr<'c> list)  =
    let opStr = stringizeBoolOperator op
    if exprs.IsEmpty then Error [(OperatorMustHaveArguments op)]
    else
        exprs 
        |> List.map (stringizeExpression db ctx)
        |> List.fold 
            (lift2Result (fun (a:string*Parametrization list) b-> 
            let strPart = fst a + (if (fst a).Length <> 0 then " " + opStr + " " else "") + (fst b)
            let parametersPart = List.concat [snd a; (snd b)]
            (strPart,parametersPart)))
            (Ok ("",[]))

and stringizeExpression (db: DB<'c>) ctx exp : Result<string*Parametrization list,ErrorMsg<'c> list> =
    match exp with
    //| ListExpr (op,exps) -> stringizeListExpr db ctx op exps
    | RelationExpr (op,e1,e2) -> stringizeRelationExpr db ctx e1 op e2
    | Not ex -> stringizeExpression db ctx ex
    |> Result.map (fun (s,p) -> (wrapExp s), p)

let stringizeSelect db (query:QueryStatement<'c>) =
    let cols = List.map (fun c -> c.Col) query.Columns
    let columnNames = cols |> List.map db.QualifiedColName
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
// Result<()
let stringizeSQLQuery db (query:QueryStatement<'c>)  =
    let ctx = ctxFactory()
    let select = stringizeSelect db query
    let where =
        match query.Condition with
        | Some cond ->
            stringizeExpression db ctx cond
            >>= ((fun a -> "WHERE " + fst a, snd a) >> Ok)
        | None -> Ok ("", [])
    map2 select where (fun s w -> (s + Environment.NewLine + fst w), snd w)

let getInsertTable db (statement : InsertStatement<'c>) =
    let tables = List.map ((fun c -> c.Column.Col) >> db.ColumnTableName) statement.Columns |> List.distinct
    match tables with
    | [] -> Error [InsertMustHaveColumns]
    | [table] -> Ok table
    | list -> Error [InsertMustTargetOneTable list]

let ensureDistinctColumns db (statement : InsertStatement<'c>) =
    let cols = statement.Columns |> List.map (fun c -> c.Column.Col)
    match cols with
    | [] -> Error InsertMustHaveColumns
    | list when list.Length = (List.distinct cols).Length -> Ok list
    | _ -> InsertMustContainDistinctColumns (cols) |> Error

let createInsertSQL db statement table =
    statement
    |> ensureDistinctColumns db
    |> function
        | Ok cols ->
            let columns =
                cols
                |> List.map db.ColumnName
                |> List.reduce (fun sum c -> sum + "," + c)
            let colParams =
                cols
                |> List.map (db.ColumnName >> (fun c -> "@" + c))
                |> List.reduce (fun sum c -> sum + "," + c)
            let sqlStr = "INSERT INTO " + table + "(" + columns + ") VALUES (" + colParams + ")"
            Ok (sqlStr, statement.Columns)
        | Error err -> Error [err]


let stringizeSQLInsert db (statement : InsertStatement<'c>) =
    getInsertTable db statement
    >>= createInsertSQL db statement


let ProjectAppColumns col = { Col = col; Type = getColumnType col }

let qualifiedColName col = getColumnTableName col + "." + getColumnName col

let projectAppDB = {ColumnName = getColumnName; ColumnTableName = getColumnTableName; ColumnType = getColumnType; QualifiedColName = qualifiedColName}
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