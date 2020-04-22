module DatabaseAccess

open Shared

open System
open System.IO
#if INTERACTIVE
let e_sqliteDir = @"c:\Users\Harmatb\.nuget\packages\sqlitepclraw.lib.e_sqlite3\2.0.2\runtimes\win10-x86\nativeassets\uap10.0\"
Environment.SetEnvironmentVariable("Path",
    Environment.GetEnvironmentVariable("Path") + ";" + e_sqliteDir)
#load @"../../.paket/load/netcoreapp3.1/Server/server.group.fsx"
#load @"../Shared/SQLAST.fs"
Directory.SetCurrentDirectory(__SOURCE_DIRECTORY__)
#endif
open System.Data
open System.Data.SQLite
open Dapper
open SQLAST
open System.Collections.Generic

type DBErrorMsg =
    | Exception of Exception
    | SQLError of SQLAST.ErrorMsg

let connectionString = 
    SQLiteConnectionStringBuilder(
        DataSource = Directory.GetCurrentDirectory() + @"\..\..\db\DevDatabase.db",
        ForeignKeys = true
        ).ToString()


let getColumns conn (tableName : string) =
    let sqlString = @"SELECT * FROM pragma_table_info(@table_name)"
    let parameters = DynamicParameters()
    parameters.Add("@table_name", tableName, Nullable DbType.String)
    Dapper.SqlMapper.Query(conn, sqlString, parameters)

let cols =
    use conn = new SQLiteConnection(connectionString)
    getColumns conn "User"

let tables =
    use connection = new SQLiteConnection(connectionString)
    let sqlString = @"SELECT * FROM SQLITE_MASTER WHERE name = @tableName"
    let parameters = DynamicParameters()
    parameters.Add("@tableName", "User", Nullable DbType.String)
    let tables = connection.Query(sqlString, parameters)
    tables

let lst = tables |> List.ofSeq

let stringize q =
    q
    |> Seq.map (fun l -> l.ToString())
    |> Seq.fold (+) ""

type SQLResult =
    | AffectedRows of int
    | ConstraintFailed of string

let deconstructDbData data =
    match data with
    | String s -> DbType.String, box s
    | Float f -> DbType.Double, box f
    | Int i -> DbType.Int32, box i

let deconstructParameter parameter =
    let parameterType, parameterValue = deconstructDbData parameter.ParamValue
    (
        (fst parameter.ParamName) + (snd parameter.ParamName |> string),
        parameterValue,
        Nullable parameterType
    )

let parametersBuilder parameters : DynamicParameters =
    let dynamicParameters = DynamicParameters()
    parameters
    |> List.map (fun parameter ->
        let pName,pVal,pType = deconstructParameter parameter
        dynamicParameters.Add(pName, pVal, pType) )
    |> ignore
    dynamicParameters

let deconstructInsertValue parameter =
    let parameterType, parameterValue = deconstructDbData parameter.Value
    (
        "@" + parameter.Column.Name,
        parameterValue,
        Nullable parameterType
    )


let insertParameterBuilder insertValues : DynamicParameters =
    let dynamicParameters = DynamicParameters()
    insertValues
    |> List.map (fun parameter ->
        let pName,pVal,pType = deconstructInsertValue parameter
        dynamicParameters.Add(pName, pVal, pType) )
    |> ignore
    dynamicParameters



let userColumns = 
    [
        ("UserName", {Table="User";Name="UserName";DataType=DBString})
        ("UserNameID",{Table="User";Name="UserNameID";DataType=DBString})
        ("PrimaryEmail",{Table="User";Name="PrimaryEmail";DataType=DBString})
    ]
    |> Map.ofSeq

[<CLIMutable>]
type DbUser ={
    UserName : string
    UserNameID : string
    PrimaryEmail : string
}

let executeQuery (sqlStr, parameters) =
    try
        use conn = new SQLiteConnection(connectionString)
        let result =conn.Query(sqlStr, parametersBuilder parameters)
        Ok result
    with
    | ex -> [Exception ex] |> Error
    
let executeQueryTyped<'T> (sqlStr, parameters) =
    try
        use conn = new SQLiteConnection(connectionString)
        let result =conn.Query<'T>(sqlStr, parametersBuilder parameters)
        Ok result
    with
    | ex -> [Exception ex] |> Error

let getUserResult userID =
    let expr = RelationExpr (Equals ,Column userColumns.["UserNameID"],userID |> String |> Value)
    let query = {Table="User";Columns=userColumns |> List.ofSeq |> List.map (fun i -> i.Value);Condition = Some expr}
    let sqlStr = stringizeSQLQuery query
    sqlStr
    |> Result.mapError (fun eList -> List.map SQLError eList)
    >>= executeQueryTyped<DbUser>

let executeInsert (sqlStr, parameters) =
    try
        use conn = new SQLiteConnection(connectionString)
        let result = conn.Execute(sqlStr, insertParameterBuilder parameters)
        Ok result
    with
    | ex -> [Exception ex] |> Error

let insertUser (user:UserInfo) =
    let insertValues = [
        {Column = userColumns.["UserName"]; Value=user.UserName |> String}
        {Column = userColumns.["UserNameID"]; Value=user.UserID |> String}
        {Column = userColumns.["PrimaryEmail"]; Value=user.UserEmail |> String}
    ]
    let insertStatement = {Columns = insertValues}
    let sqlString = stringizeSQLInsert insertStatement
    sqlString
    |> Result.mapError (fun eList -> List.map SQLError eList)
    >>= executeInsert