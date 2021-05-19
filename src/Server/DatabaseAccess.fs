module DatabaseAccess


open System
open System.IO
open System.Collections.Generic
open System.Linq

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
open Shared
open ResultExtensions
open SQLAST
open SQLGenerator
open DatabaseSchema
open Security
open ProjectSpecificLabels

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

let deconstructInsertValue db parameter =
    let parameterType, parameterValue = deconstructDbData parameter.Value
    (
        "@" + (db.GetColumnName parameter.Column.Col),
        parameterValue,
        Nullable parameterType
    )


let insertParameterBuilder db insertValues : DynamicParameters =
    let dynamicParameters = DynamicParameters()
    insertValues
    |> List.map (fun parameter ->
        let pName,pVal,pType = deconstructInsertValue db parameter
        dynamicParameters.Add(pName, pVal, pType) )
    |> ignore
    dynamicParameters

[<CLIMutable>]
type DbUser ={
    UserID : int
    UserName : string
    UserNameID : string
    PrimaryEmail : string
    GivenName : string
    FamilyName : string
    UserRole : string
    QueryLimit : string
}

module UserRole =
    let private admin = "Admin"
    let private user = "User"
    let persist role =
        match role with
        | Admin -> admin
        | User -> user
        | Nothing -> "Nothing"
    let read roleString =
        match roleString with
        | a when a = admin -> Admin
        | u when u = user -> User
        | _ -> Nothing

let dbUserToUserInfo dbUser : UserInfo =
    {
        UserName = dbUser.UserName
        UserNameID = dbUser.UserNameID
        GivenName = dbUser.GivenName
        FamilyName = dbUser.FamilyName
        PrimaryEmail = dbUser.PrimaryEmail
        UserRole = UserRole.read dbUser.UserRole
        QueryLimit = dbUser.QueryLimit
    }

[<CLIMutable>]
type DbLocalAuth ={
    UserID : string
    UserRole : string
    QueryLimit : string
    PasswordHash : string
    Salt : string
}

let executeQuery (sqlStr, parameters) =
    try
        use conn = new SQLiteConnection(connectionString)
        let result =conn.Query(sqlStr, parametersBuilder parameters)
        Ok result
    with
    | ex -> [DBException ex] |> Error

let executeQueryTyped<'T> (sqlStr, parameters) =
    try
        use conn = new SQLiteConnection(connectionString)
        let result =conn.Query<'T>(sqlStr, parametersBuilder parameters)
        Ok result
    with
    | ex -> [DBException ex] |> Error

let oneOrNoneQuery (result:Result<IEnumerable<'T>,_>) =
    match result with
    | Ok one when one.Count() = 1 -> one.First() |> Some |> Ok
    | Ok none when none.Count() = 0 -> None |> Ok
    | Ok _ -> [MoreThanOneResult] |> Error
    | Error error -> Error error

let alwaysOneQuery (result:Result<IEnumerable<'T>,_>) =
    match result with
    | Ok one when one.Count() = 1 -> one.First() |> Ok
    | Ok none when none.Count() = 0 -> [MissingData] |> Error
    | Ok _ -> [MoreThanOneResult] |> Error
    | Error error -> Error error


let getUserByName userName =
    let expr = RelationExpr (
                UserName |> UserCol |> getColumn |> Column,
                Equals,
                userName |> String |> Value
                )
    let queryColumns = [
        UserCol UserID        |> getColumn
        UserCol UserName        |> getColumn
        UserCol UserNameID      |> getColumn
        UserCol PrimaryEmail    |> getColumn
        UserCol GivenName       |> getColumn
        UserCol FamilyName      |> getColumn
    ]
    let query = {Columns=queryColumns; Condition = Some expr}
    let sqlStr = stringizeSQLQuery projectAppDBSchema query
    sqlStr
    |> Result.mapError (fun eList -> List.map SQLError eList)
    >>= executeQueryTyped<DbUser>

let mapDBErrorToAPIError dbErr =
    Result.mapError (List.map DBError) dbErr

let userInfoFromAuthInfo()  =
    let innerFn (userAuthInfo: UserAuthInfo) : UserInfoResult =
        getUserByName userAuthInfo.UserName
        |> alwaysOneQuery
        |>> dbUserToUserInfo
        |> mapDBErrorToAPIError
    innerFn

let executeInsert db (sqlStr, parameters) =
    try
        use conn = new SQLiteConnection(connectionString)
        let result = conn.Execute(sqlStr, insertParameterBuilder db parameters)
        Ok result
    with
    | ex -> [DBException ex] |> Error

let ensureInsert expectedCount insertResult =
    match insertResult with
    | correct when correct = expectedCount -> Ok insertResult
    | _ -> Error [InsertFailed]
