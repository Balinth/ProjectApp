module DatabaseSchema

open SQLAST

type ProjectAppTable =
    | UserTable
    | ProjectTable

type UserCol =
    | UserName
    | UserID
    | GivenName
    | FamilyName
    | PrimaryEmail
    | UserNameID

type ProjectCol =
    | ProjectName
    | ProjectID
    | StartDate
    | Code

type ProjectAppCol =
    | UserCol of UserCol
    | ProjectTableCol of ProjectCol

let getColumnTable col =
    match col with
    | UserCol _ -> ProjectAppTable.UserTable
    | ProjectTableCol _ -> ProjectAppTable.ProjectTable

let getTableName table : string =
    match table with
    | ProjectAppTable.UserTable _ -> "User"
    | ProjectAppTable.ProjectTable _ -> "Project"

let getColumnName (col : ProjectAppCol) : string =
    match col with
    | UserCol u ->
        match u with
        | UserName -> "UserName"
        | PrimaryEmail -> "PrimaryEmail"
        | UserNameID -> "UserNameID"
        | UserID -> "UserID"
        | GivenName -> "GivenName"
        | FamilyName -> "FamilyName"
    | ProjectTableCol p ->
        match p with
        | ProjectName -> "ProjectName"
        | ProjectID -> "ProjectID"
        | StartDate -> "StartDate"
        | Code -> "Code"

let getColumnType col =
    match col with
    | UserCol u ->
        match u with
        | UserName -> DBString
        | PrimaryEmail -> DBString
        | UserNameID -> DBString
        | UserID -> DBInt
        | GivenName -> DBString
        | FamilyName -> DBString
    | ProjectTableCol p ->
        match p with
        | ProjectName -> DBString
        | ProjectID -> DBString
        | StartDate -> DBInt
        | Code -> DBInt

let getColumn col = {Col=col;Type=getColumnType col}

let projectAppDBSchema = {
    GetColumnTable = getColumnTable
    GetTableName = getTableName
    GetColumnName = getColumnName
    GetColumnType = getColumnType
}