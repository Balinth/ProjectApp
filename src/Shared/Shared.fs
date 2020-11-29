namespace Shared

open System

open DatabaseSchema
open DynamicTable

type Counter = { Value : int }

type UserInfo = {
    UserName : string
    UserNameID : string
    GivenName : string
    FamilyName : string
    UserEmail : string
}

type ClaimError =
    | NoSuchClaim of string
    | ClaimHadNullValue of string

type DBError<'column,'table> =
    | DBException of Exception
    | SQLError of SQLAST.ErrorMsg<'column,'table>
    | InsertFailed

type APIError =
    | ClaimError of ClaimError
    | DBError of DBError<ProjectAppCol,ProjectAppTable>

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type ISecureAPI = {
    logIn : unit -> Async<Result<UserInfo,APIError list>>
    logOut : unit -> Async<Result<UserInfo,APIError list>>
    getUserDetails : unit -> Async<Result<UserInfo,APIError list>>
    getTable : unit -> Async<DynamicTable._T option>
}

