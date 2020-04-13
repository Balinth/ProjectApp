namespace Shared

type Counter = { Value : int }

type UserInfo = {
    UserName : string
    UserID : string
    UserEmail : string
}

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type ISecureAPI = {
    logIn : unit -> Async<UserInfo option>
    logOut : unit -> Async<UserInfo option>
    getUserDetails : unit -> Async<UserInfo option>  
}

