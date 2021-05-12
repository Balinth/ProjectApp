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
}

let dbUserToUserInfo dbUser : UserInfo =
    {
        UserName = dbUser.UserName
        UserNameID = dbUser.UserNameID
        GivenName = dbUser.GivenName
        FamilyName = dbUser.FamilyName
        PrimaryEmail = dbUser.PrimaryEmail
    }

[<CLIMutable>]
type DbLocalAuth ={
    UserID : string
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

let queryLocalAuth userName =
    let getLocalAuthQuery = {
        Columns = [
            UserCol UserID
            LocalAuthenticationCol Salt
            LocalAuthenticationCol PasswordHash
        ] |> List.map getColumn
        Condition =
            Some (
                BinaryBoolExpr (
                    RelationExpr (
                        UserCol UserName |> getColumn |> Column,
                        Equals,
                        userName |> String |> Value
                    ),
                    And,
                    RelationExpr (
                        UserCol UserID |> getColumn |> Column,
                        Equals,
                        LocalAuthenticationCol User_ID |> getColumn |> Column
                    )
                )
            )
        }
    let sqlStr = stringizeSQLQuery projectAppDBSchema getLocalAuthQuery
    sqlStr
    |> Result.mapError (fun eList -> List.map SQLError eList)
    >>= executeQueryTyped<DbLocalAuth>

let queryUserNameTaken userName =
    {
        Columns = [UserCol UserName |> getColumn]
        Condition = Some
            (
            RelationExpr(
                UserCol UserName |> getColumn |> Column,
                Equals,
                userName |> String |> Value
            )
        )
    }
    |> stringizeSQLQuery projectAppDBSchema
    |> Result.mapError (fun eList -> List.map SQLError eList)
    >>= executeQuery
    |>> (fun objs -> objs.Count() = 1)

let createInsert table columnsAndData =
    let columns = List.map fst columnsAndData
    let data = List.map snd columnsAndData
    InsertStatement.create projectAppDBSchema table columns data

let register (registerInfo:RegisterInfo) : RegistrationResult =
    System.Environment.GetEnvironmentVariables().Keys
    |> Seq.cast
    |> Seq.iter (fun o -> printfn "%A" o)
    match Validation.validateUsername registerInfo.UserName, Validation.validatePassword registerInfo.Password with
    | Ok userName, Ok password ->
        let insertUserValues = [
            UserCol UserName , userName |> String
            UserCol GivenName, registerInfo.GivenName |> String
            UserCol FamilyName, registerInfo.FamilyName |> String
            UserCol PrimaryEmail, registerInfo.UserEmail |> String
            UserCol UserNameID, Guid.NewGuid().ToString() |> String
        ]
        let insertUserResult =
            createInsert UserTable insertUserValues
            >>= stringizeSQLInsert projectAppDBSchema
            |> Result.mapError (fun eList -> List.map SQLError eList)
            >>= executeInsert projectAppDBSchema

        match insertUserResult with
        | Error someErr ->
            // check if the problem is a taken userName
            match queryUserNameTaken registerInfo.UserName with
            | Ok result when result = true -> UserNameTaken |> Error
            | Ok _ -> [ InsertFailed |> DBError ] |> APIError |> Error
            | Error moreErrors -> (moreErrors @ someErr) |> List.map DBError |> APIError |> Error
        | Ok _ ->
            let salt = createRandomKey()
            let password = utf8Bytes registerInfo.Password
            let saltedPassword = Array.concat [salt; password]
            let passwordHash = sha256Hash saltedPassword


            result {
            let! dbUser = getUserByName registerInfo.UserName |> alwaysOneQuery
            let! insertStatement =
                createInsert LocalAuthenticationTable [
                    LocalAuthenticationCol User_ID, dbUser.UserID |> Int
                    LocalAuthenticationCol PasswordHash, base64 passwordHash |> String
                    LocalAuthenticationCol Salt, base64 salt |> String
                ]
                |> Result.mapError (fun eList -> List.map SQLError eList)
            let! sqlString = stringizeSQLInsert projectAppDBSchema insertStatement
            let! insertResult =
                executeInsert projectAppDBSchema sqlString
                >>= (ensureInsert 1)
            return! userName |> Shared.UserName |> Ok
            }
            |> Result.mapError (List.map DBError >> APIError)
    | Error userNameProblems, Error passwordProblems -> ValidationError (passwordProblems,userNameProblems) |> Error
    | Error userNameProblems, _ -> ValidationError ([],userNameProblems) |> Error
    | _, Error passwordProblems -> ValidationError (passwordProblems,[]) |> Error


let login (loginInfo:LoginInfo) =
    match queryLocalAuth loginInfo.UserName |> oneOrNoneQuery with
    | Ok (Some dbAuth) ->
        if Security.verifyPassword loginInfo.Password dbAuth.Salt dbAuth.PasswordHash
        then
            let userName : UserAuthInfo = {UserName = loginInfo.UserName}
            let token = encodeJwt userName
            let userDetails =
                getUserByName loginInfo.UserName
                |> alwaysOneQuery
                |>> dbUserToUserInfo
                |> mapDBErrorToAPIError
            match userDetails with
            | Ok userDetails ->
                (Token token, userDetails) |> Ok
            | Error errors -> Error (UnexpectedLoginError (sprintf "%A" errors))
        else
            Error PasswordIncorrect
    | Ok None -> Error UsernameDoesNotExist
    | Error error -> UnexpectedLoginError (sprintf "%A" error) |> Error

let insertUser (user:UserInfo) =
    let insertValues = [
        UserCol UserName , user.UserName |> String
        UserCol UserNameID, user.UserNameID |> String
        UserCol PrimaryEmail, user.PrimaryEmail |> String
        UserCol GivenName, user.GivenName |> String
        UserCol FamilyName, user.FamilyName |> String
    ]
    //let insertStatement =
    createInsert UserTable insertValues
    >>= stringizeSQLInsert projectAppDBSchema
    |> Result.mapError (fun eList -> List.map SQLError eList)
    >>= executeInsert projectAppDBSchema