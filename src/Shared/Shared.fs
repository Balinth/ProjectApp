namespace Shared

open System

open DatabaseSchema
open DynamicTable
open ParserCombinator

type Counter = { Value : int }

type LoginInfo = {
    UserName : string
    Password : string
}

type Token = Token of string

//  This module uses the JOSE-JWT library https://github.com/dvsekhvalnov/jose-jwt
type UserRole =
    | Admin
    | User
    | Nothing

type UserInfo = {
    UserName : string
    UserNameID : string
    GivenName : string
    FamilyName : string
    PrimaryEmail : string
    UserRole : UserRole
    QueryLimit : string
}

type RegisterInfo = {
    UserName : string
    GivenName : string
    FamilyName : string
    UserEmail : string
    Password : string
}

type AuthError =
    | TokenInvalid
    | UserUnauthorized

type LoginError =
    | UsernameDoesNotExist
    | PasswordIncorrect
    | UnexpectedLoginError of string

type LoginResult = Result<Token * UserInfo,LoginError>

type DBError<'column,'table> =
    | DBException of Exception
    | SQLError of SQLAST.ErrorMsg<'column,'table>
    | InsertFailed
    | MoreThanOneResult
    | MissingData

type APIError =
    | AuthError of AuthError
    | DBError of DBError<ProjectAppCol,ProjectAppTable>
    | ParserError of BasicLabel*BasicParserError*ParserPosition
    | UnexpectedError of exn

type RegistrationError =
    | UserNameTaken
    | ValidationError of
        passwordProblem: Validation.ValidationError list
        * userNameProblem: Validation.ValidationError list
    | APIError of APIError list
    | UnexpectedRegistrationError of string

type ClientError =
    | UserInfoWithoutToken

type UserName = UserName of string

type RegistrationResult = Result<UserName,RegistrationError>

type UserInfoResult = Result<UserInfo,APIError list>

type QueryResult = Result<DynamicTable<ProjectAppCol>,APIError list>

type QueryInfo = {
    Name: string
    Query: string
    SavedBy: string option
}

type SecureRequest<'t> = {
    Token : Token
    Body : 't
}
type SecureResponse<'t> = Async<Result<'t, APIError list>>

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type ISecureAPI = {
    register : RegisterInfo -> Async<RegistrationResult>
    login : LoginInfo -> Async<LoginResult>
    getUserDetails : SecureRequest<unit> -> SecureResponse<UserInfoResult>
    query : SecureRequest<string> -> SecureResponse<QueryResult>
    saveQuery : SecureRequest<QueryInfo> -> SecureResponse<string>
    deleteQuery : SecureRequest<QueryInfo> -> SecureResponse<string>
    listSavedQueries : SecureRequest<unit> -> SecureResponse<QueryInfo list>
    modifyUser : SecureRequest<UserInfo> -> SecureResponse<UserInfo>
    insert : SecureRequest<string> -> SecureResponse<unit>
}

