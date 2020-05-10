module DatabaseSchema

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

type ProjectAppColumn =
    | UserTable of UserTable
    | ProjectTable of ProjectTable

let getColumnName (col : ProjectAppColumn) : string =
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

let getColumnTableName (col : ProjectAppColumn) : string =
    match col with
    | UserTable U -> "User"
    | ProjectTable P -> "Project"