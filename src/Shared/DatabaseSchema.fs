module DatabaseSchema
open System

type UserTable =
    | UserName
    | UserID
    | PrimaryEmail
    | UserNameID

type ProjectTable =
    | ProjectName
    | ProjectID
    | StartDate
    | Code

type ProjectAppDB =
    | UserTable of UserTable
    | ProjectTable of ProjectTable


let getColumnName (col : ProjectAppDB) : string =
    match col with
    | UserTable u ->
        match u with
        | UserName -> "UserName"
        | PrimaryEmail -> "UserEmail"
        | UserNameID -> "UserNameID"
        | UserID -> "UserID"
    | ProjectTable p ->
        match p with
        | ProjectName -> "ProjectName"
        | ProjectID -> "ProjectID"
        | StartDate -> "StartDate"
        | Code -> "Code"

let getColumnType (col : ProjectAppDB) : Type =
    match col with
    | UserTable u ->
        match u with
        | UserName -> typeof<string>
        | PrimaryEmail -> typeof<string>
        | UserNameID -> typeof<string>
        | UserID -> typeof<int>
    | ProjectTable p ->
        match p with
        | ProjectName -> typeof<string>
        | ProjectID -> typeof<string>
        | StartDate -> typeof<int>
        | Code -> typeof<int>

let getColumnTableName (col : ProjectAppDB) : string =
    match col with
    | UserTable U -> "User"
    | ProjectTable P -> "Project"

let testCol : ProjectAppDB = UserName |> UserTable

let testColName = getColumnName testCol