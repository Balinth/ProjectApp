module DatabaseSchema

open SQLAST

// fsharplint:disable TypePrefixing

type ProjectAppTable =
    | ProjectTable
    | PhaseTable
    | UserTable
    | ForFabricationTable
    | LocalAuthenticationTable

type ProjectCol =
    | ProjectName
    | ProjectID
    | StartDate
    | Code

type PhaseCol =
    | PhaseID
    | PhaseName
    | Revision
    | Project_ID
    | ForFabrication_ID

type UserCol =
    | UserID
    | UserName
    | GivenName
    | FamilyName
    | PrimaryEmail
    | UserNameID

type ForFabricationCol =
    | ForFabricationID
    | SenderUser_ID
    | EmailOriginal
    | SentDate

type LocalAuthenticationCol =
    | LocalAuthID
    | PasswordHash
    | Salt
    | User_ID

type ProjectAppCol =
    | ProjectTableCol of ProjectCol
    | PhaseCol of PhaseCol
    | UserCol of UserCol
    | ForFabricationCol of ForFabricationCol
    | LocalAuthenticationCol of LocalAuthenticationCol

let getColumnTable col =
    match col with
    | ProjectTableCol _ -> ProjectTable
    | PhaseCol(_) -> PhaseTable
    | UserCol _ -> UserTable
    | ForFabricationCol(_) -> ForFabricationTable
    | LocalAuthenticationCol(_) -> LocalAuthenticationTable

let getTableName table : string =
    match table with
    | UserTable _ -> "User"
    | ProjectTable _ -> "Project"
    | PhaseTable -> "Phase"
    | ForFabricationTable -> "ForFabrication"
    | LocalAuthenticationTable -> "LocalAuthentication"

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
    | PhaseCol p ->
        match p with
        | PhaseID -> "PhaseID"
        | PhaseName -> "PhaseName"
        | Revision -> "Revision"
        | Project_ID -> "Project_ID"
        | ForFabrication_ID -> "ForFabrication_ID"
    | ForFabricationCol forFabCol ->
        match forFabCol with
        | ForFabricationID -> "ForFabricationID"
        | SenderUser_ID -> "SenderUser_ID"
        | EmailOriginal -> "EmailOriginal"
        | SentDate -> "SentDate"
    | LocalAuthenticationCol localAuthCol ->
        match localAuthCol with
        | LocalAuthID -> "LocalAuthID"
        | PasswordHash -> "PasswordHash"
        | User_ID -> "User_ID"
        | Salt -> "Salt"

let getColumnType col =
    match col with
    | UserCol userCol ->
        match userCol with
        | UserName -> DBString
        | PrimaryEmail -> DBString
        | UserNameID -> DBString
        | UserID -> DBInt
        | GivenName -> DBString
        | FamilyName -> DBString
    | ProjectTableCol projectCol ->
        match projectCol with
        | ProjectName -> DBString
        | ProjectID -> DBInt
        | StartDate -> DBInt
        | Code -> DBString
    | PhaseCol phaseCol ->
        match phaseCol with
        | PhaseID -> DBInt
        | PhaseName -> DBString
        | Revision -> DBString
        | Project_ID -> DBInt
        | ForFabrication_ID -> DBInt
    | ForFabricationCol forFabCol ->
        match forFabCol with
        | ForFabricationID -> DBInt
        | SenderUser_ID -> DBInt
        | EmailOriginal -> DBString
        | SentDate -> DBInt
    | LocalAuthenticationCol localAuthCol ->
        match localAuthCol with
        | User_ID -> DBInt
        | LocalAuthID -> DBInt
        | PasswordHash -> DBString
        | Salt -> DBString

let getColumn col = {Col=col;Type=getColumnType col}

let projectAppDBSchema = {
    GetColumnTable = getColumnTable
    GetTableName = getTableName
    GetColumnName = getColumnName
    GetColumnType = getColumnType
}