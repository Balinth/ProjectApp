module CRUD

open System
open System.IO
open System.Collections.Generic
open System.Linq

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
open DatabaseAccess

let stringizeSQLInsert = stringizeSQLInsert projectAppDBSchema

let queryLocalAuth userName =
    let getLocalAuthQuery = {
        Columns = [
            UserCol UserID
            UserCol UserRole
            UserCol QueryLimit
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
            UserCol UserRole, UserRole.User |> UserRole.persist |> String
            UserCol QueryLimit, "" |> String
        ]
        let insertUserResult =
            createInsert UserTable insertUserValues
            >>= stringizeSQLInsert
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
            let! sqlString = SQLGenerator.stringizeSQLInsert projectAppDBSchema insertStatement
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
            let userName : UserAuthInfo = {
                UserName = loginInfo.UserName
                Role = UserRole.read dbAuth.UserRole
                QueryLimit = dbAuth.QueryLimit
                }
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
    >>= stringizeSQLInsert
    |> Result.mapError (fun eList -> List.map SQLError eList)
    >>= executeInsert projectAppDBSchema


let saveQuery (query: QueryInfo) (user:UserAuthInfo) =

    match getUserByName user.UserName |> oneOrNoneQuery with
    | Ok (Some dbUser) ->
        let insertValues = [
            SavedQueryCol QueryName, query.Name |> String
            SavedQueryCol Query, query.Query |> String
            SavedQueryCol SavedBy, dbUser.UserName |> String
        ]
        createInsert SavedQueryTable insertValues
        >>= stringizeSQLInsert
        |> Result.mapError (fun eList -> List.map SQLError eList)
        >>= executeInsert projectAppDBSchema
        |> Result.map (fun _ -> query.Name)
        |> Result.mapError (List.map DBError)
    | Ok None -> [InsertFailed |> DBError] |> Error
    | Error error -> List.map DBError error |> Error

[<CLIMutable>]
type DbSavedQuery = {
    QueryName : string
    Query : string
    SavedBy : string
}

let dbSavedQueryToQueryInfo query : QueryInfo =
    {Name = query.QueryName; Query = query.Query; SavedBy = Some query.SavedBy}

let listSavedQueries () (user:UserAuthInfo) =
    let queryColumns = [
        SavedQueryCol QueryName        |> getColumn
        SavedQueryCol Query      |> getColumn
        SavedQueryCol SavedBy      |> getColumn
    ]
    let query = {
        Columns=queryColumns
        Condition =
            Some(
                RelationExpr(
                    SavedQueryCol SavedBy |> getColumn |> Column,
                    Equals,
                    user.UserName |> String |> Value
                )
        )
        }
    let sqlStr = stringizeSQLQuery projectAppDBSchema query
    sqlStr
    |> Result.mapError (fun eList -> List.map SQLError eList)
    >>= executeQueryTyped<DbSavedQuery>
    |> Result.map (
        Seq.map dbSavedQueryToQueryInfo
        >> List.ofSeq
    )
    |> Result.mapError (List.map DBError)

let deleteQuery query user = failwith "NotImplemented"

let modifyUser requesterUser targetUser : UserInfo = failwith "NotImplemented"