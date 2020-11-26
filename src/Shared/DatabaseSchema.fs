module DatabaseSchema

open SQLAST

type UserTable =
    | UserName
    | UserID
    | GivenName
    | FamilyName
    | PrimaryEmail
    | UserNameID

type ProjectTable =
    | ProjectName
    | ProjectID
    | StartDate
    | Code

type ProjectAppColumn =
    | UserTable of UserTable
    | ProjectTable of ProjectTable

let getColumnName (col : ProjectAppColumn) : string =
    match col with
    | UserTable u ->
        match u with
        | UserName -> "UserName"
        | PrimaryEmail -> "PrimaryEmail"
        | UserNameID -> "UserNameID"
        | UserID -> "UserID"
        | GivenName -> "GivenName"
        | FamilyName -> "FamilyName"
    | ProjectTable p ->
        match p with
        | ProjectName -> "ProjectName"
        | ProjectID -> "ProjectID"
        | StartDate -> "StartDate"
        | Code -> "Code"

let getColumnTableName (col : ProjectAppColumn) : string =
    match col with
    | UserTable _ -> "User"
    | ProjectTable _ -> "Project"

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

let getColumn col = {Col=col;Type=getColumnType col}