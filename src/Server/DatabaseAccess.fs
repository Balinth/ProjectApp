module DatabaseAccess

open Shared

open System
#if INTERACTIVE
let e_sqliteDir = @"c:\Users\Harmatb\.nuget\packages\sqlitepclraw.lib.e_sqlite3\2.0.2\runtimes\win10-x86\nativeassets\uap10.0\"
Environment.SetEnvironmentVariable("Path",
    Environment.GetEnvironmentVariable("Path") + ";" + e_sqliteDir)
#load @"../../.paket/load/netcoreapp3.1/Server/server.group.fsx"
#load @"../Shared/SQLAST.fs"
#endif
open System.Data
open System.Data.SQLite
open Dapper
open System.IO
open SQLAST

let connectionString = 
    SQLiteConnectionStringBuilder(
        DataSource = Directory.GetCurrentDirectory() + @"\db\DevDatabase.db",
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

stringize cols

type dbUser = {
    UserName : string
    PrimaryEmail : string
    UserNameID : string
}



type SQLResult =
    | AffectedRows of int
    | ConstraintFailed of string

let insertUser user =
    try
        use conn = new SQLiteConnection(connectionString)
        let sqlString = "INSERT INTO User (UserName,PrimaryEmail,UserNameID) Values (@UserName,@PrimaryEmail,@UserNameID)"
        let parameters = DynamicParameters()
        parameters.Add("@UserName", user.UserName, Nullable DbType.String)
        parameters.Add("@PrimaryEmail", user.PrimaryEmail, Nullable DbType.String)
        parameters.Add("@UserNameID", user.PrimaryEmail, Nullable DbType.String)
        conn.Execute(sqlString, parameters)
        |> AffectedRows
    with
    | :? SQLiteException as e when e.ResultCode = SQLiteErrorCode.Constraint -> ConstraintFailed e.Message

let user = {UserName = null; UserNameID = "tefdasstGUIDfsgdfsfdadsfa"; PrimaryEmail = "tefasst@test.testfdasfaf"}

let affected = insertUser user

let deconstructParameter parameter =
    let parameterType, parameterValue =
        match parameter.ParamValue with
        | String s -> DbType.String, box s
        | Float f -> DbType.Double, box f
        | Int i -> DbType.Int32, box i
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

let getUser userID =
    let userColumns = 
        [
            ("UserName", {Table="User";Name="UserName";DataType=DBString})
            ("UserNameID",{Table="User";Name="UserNameID";DataType=DBString})
            ("PrimaryEmail",{Table="User";Name="PrimaryEmail";DataType=DBString})
        ]
        |> Map.ofSeq
    let expr = RelationExpr (Equals ,Column userColumns.["UserName"],"test" |> String |> Value)
    let query = {Table="User";Columns=userColumns |> List.ofSeq |> List.map (fun i -> i.Value);Condition = Some expr}
    let sqlStr = stringizeSQL query
    use conn = new SQLiteConnection(connectionString)
    match sqlStr with
    | Ok (s, parameters) ->
        printfn "%s" s
        conn.Query(s, parametersBuilder parameters)
    | Error err -> err |> string |> failwith

let test = getUser "test"

test |> Seq.map (fun r -> r.ToString())