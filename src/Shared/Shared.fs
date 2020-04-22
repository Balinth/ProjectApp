namespace Shared

type Counter = { Value : int }

type UserInfo = {
    UserName : string
    UserID : string
    UserEmail : string
}

type ClaimError =
    | NoSuchClaim of string
    | ClaimHadNullValue of string

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type ISecureAPI = {
    logIn : unit -> Async<Result<UserInfo,ClaimError list>>
    logOut : unit -> Async<Result<UserInfo,ClaimError list>>
    getUserDetails : unit -> Async<Result<UserInfo,ClaimError list>>  
}

