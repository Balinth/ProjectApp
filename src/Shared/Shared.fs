namespace Shared

open System

open DatabaseSchema
open DynamicTable

type Counter = { Value : int }

type LoginInfo = {
    UserName : string
    Password : string
}

type UserInfo = {
    UserName : string
    UserNameID : string
    GivenName : string
    FamilyName : string
    PrimaryEmail : string
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

type LoginResult = Result<string,LoginError>

type DBError<'column,'table> =
    | DBException of Exception
    | SQLError of SQLAST.ErrorMsg<'column,'table>
    | InsertFailed
    | MoreThanOneResult
    | MissingData

type APIError =
    | AuthError of AuthError
    | DBError of DBError<ProjectAppCol,ProjectAppTable>

type RegistrationError =
    | UserNameTaken
    | ValidationError of
        passwordProblem: Validation.ValidationError list
        * userNameProblem: Validation.ValidationError list
    | APIError of APIError list

type ClientError =
    | UserInfoWithoutToken

type RegistrationResult = Result<string,RegistrationError>

type UserInfoResult = Result<UserInfo,APIError list>

type SecureRequest<'t> = {
    Token : string
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
}

