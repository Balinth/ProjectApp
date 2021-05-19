module DatabaseSchema

open SQLAST

// fsharplint:disable TypePrefixing

type ProjectAppTable =
    | ProjectTable
    | PhaseTable
    | UserTable
    | ForFabricationTable
    | LocalAuthenticationTable
    | OrganizationTable
    | OrganizationMemberTable
    | DesignerTable
    | DrafterTable
    | SavedQueryTable

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
    | Mass

type UserCol =
    | UserID
    | UserName
    | GivenName
    | FamilyName
    | PrimaryEmail
    | UserNameID
    | UserRole
    | QueryLimit

type OrganizationCol =
    | OrganizationID
    | OrganizationName

type OrganizationMemberCol =
    | User_ID
    | Organization_ID

type ForFabricationCol =
    | ForFabricationID
    | SenderUser_ID
    | EmailOriginal
    | SentDate

type DesignerCol =
    | User_ID
    | Phase_ID
    | Phase_ProjectID
    | Phase_Revision

type DrafterCol =
    | User_ID
    | Phase_ID
    | Phase_ProjectID
    | Phase_Revision

type LocalAuthenticationCol =
    | LocalAuthID
    | PasswordHash
    | Salt
    | User_ID

type SavedQueryCol =
    | QueryName
    | Query
    | SavedBy

type ProjectAppCol =
    | ProjectTableCol of ProjectCol
    | PhaseCol of PhaseCol
    | UserCol of UserCol
    | ForFabricationCol of ForFabricationCol
    | LocalAuthenticationCol of LocalAuthenticationCol
    | OrganizationCol of OrganizationCol
    | OrganizationMemberCol of OrganizationMemberCol
    | DrafterCol of DrafterCol
    | DesignerCol of DesignerCol
    | SavedQueryCol of SavedQueryCol

let getColumnTable col =
    match col with
    | ProjectTableCol _ -> ProjectTable
    | PhaseCol(_) -> PhaseTable
    | UserCol _ -> UserTable
    | ForFabricationCol(_) -> ForFabricationTable
    | LocalAuthenticationCol(_) -> LocalAuthenticationTable
    | OrganizationCol(_) -> OrganizationTable
    | OrganizationMemberCol(_) -> OrganizationMemberTable
    | DrafterCol(_) -> DrafterTable
    | DesignerCol(_) -> DesignerTable
    | SavedQueryCol(_) -> SavedQueryTable

let getTableName table : string =
    match table with
    | UserTable _ -> "User"
    | ProjectTable _ -> "Project"
    | PhaseTable -> "Phase"
    | ForFabricationTable -> "ForFabrication"
    | LocalAuthenticationTable -> "LocalAuthentication"
    | OrganizationTable -> "Organization"
    | OrganizationMemberTable -> "OrganizationMember"
    | DesignerTable -> "Designer"
    | DrafterTable -> "Drafter"
    | SavedQueryTable -> "SavedQuery"

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
        | UserRole -> "UserRole"
        | QueryLimit -> "QueryLimit"
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
        | Mass -> "Mass"
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
    | OrganizationCol orgCol ->
        match orgCol with
        | OrganizationID -> "OrganizationID"
        | OrganizationName -> "OrganizationName"
    | OrganizationMemberCol orgMemberCol ->
        match orgMemberCol with
        | Organization_ID -> "Organization_ID"
        | OrganizationMemberCol.User_ID -> "User_ID"
    | DrafterCol drafterCol ->
        match drafterCol with
        | DrafterCol.User_ID -> "User_ID"
        | DrafterCol.Phase_ID -> "Phase_ID"
        | DrafterCol.Phase_ProjectID -> "Phase_ProjectID"
        | DrafterCol.Phase_Revision -> "Phase_Revision"
    | DesignerCol designerCol ->
        match designerCol with
        | DesignerCol.User_ID -> "User_ID"
        | DesignerCol.Phase_ID -> "Phase_ID"
        | DesignerCol.Phase_ProjectID -> "Phase_ProjectID"
        | DesignerCol.Phase_Revision -> "Phase_Revision"
    | SavedQueryCol savedQueryCol ->
        match savedQueryCol with
        | QueryName -> "QueryName"
        | Query -> "Query"
        | SavedBy -> "SavedBy"

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
        | UserRole -> DBString
        | QueryLimit -> DBString
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
        | Mass -> DBFloat
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
    | OrganizationCol orgCol ->
        match orgCol with
        | OrganizationID -> DBInt
        | OrganizationName -> DBString
    | OrganizationMemberCol orgMemberCol ->
        match orgMemberCol with
        | Organization_ID -> DBInt
        | OrganizationMemberCol.User_ID -> DBInt
    | DrafterCol drafterCol ->
        match drafterCol with
        | DrafterCol.User_ID -> DBInt
        | DrafterCol.Phase_ID -> DBInt
        | DrafterCol.Phase_ProjectID -> DBInt
        | DrafterCol.Phase_Revision -> DBString
    | DesignerCol designerCol ->
        match designerCol with
        | DesignerCol.User_ID -> DBInt
        | DesignerCol.Phase_ID -> DBInt
        | DesignerCol.Phase_ProjectID -> DBInt
        | DesignerCol.Phase_Revision -> DBString
    | SavedQueryCol savedQueryCol ->
        match savedQueryCol with
        | QueryName -> DBString
        | Query -> DBString
        | SavedBy -> DBString

let getColumn col = {Col=col;Type=getColumnType col}

let projectAppDBSchema = {
    GetColumnTable = getColumnTable
    GetTableName = getTableName
    GetColumnName = getColumnName
    GetColumnType = getColumnType
}