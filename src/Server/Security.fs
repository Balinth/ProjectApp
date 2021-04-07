module Security

open System
open System.IO
open System.Text
open Newtonsoft.Json
open System.Security.Cryptography
open Shared


//  This module uses the JOSE-JWT library https://github.com/dvsekhvalnov/jose-jwt

type UserAuthInfo = {
    UserName: string
}

let createRandomKey() =
    let generator = System.Security.Cryptography.RandomNumberGenerator.Create()
    let randomKey = Array.init 32 byte
    generator.GetBytes(randomKey)
    randomKey

/// A pass phrase you create only once and save to a file on the server
/// The next time the server runs, the pass phrase is read and used
let private passPhrase =
    Convert.FromBase64String(Environment.GetEnvironmentVariable "ServerSecret")

let private encodeString (payload : string) =
    Jose.JWT.Encode(payload, passPhrase, Jose.JweAlgorithm.A256KW, Jose.JweEncryption.A256CBC_HS512)
let private decodeString (jwt : string) =
    Jose.JWT.Decode(jwt, passPhrase, Jose.JweAlgorithm.A256KW, Jose.JweEncryption.A256CBC_HS512)

/// Encodes an object as a JSON web token.
let encodeJwt token = JsonConvert.SerializeObject token |> encodeString

/// Decodes a JSON web token to an object.
let decodeJwt<'a> (jwt : string) : 'a = decodeString jwt |> JsonConvert.DeserializeObject<'a>

/// Returns some userinfo if the JSON Web Token is successfully decoded and the signature is verified.
let validateJwt (jwt : string) : UserAuthInfo option =
    try
        let token = decodeJwt jwt
        Some token
    with _ -> None

let utf8Bytes (input : string) = Encoding.UTF8.GetBytes(input)
let base64 (input : byte []) = Convert.ToBase64String(input)
let sha256 = SHA256.Create()
let sha256Hash (input : byte []) : byte [] = sha256.ComputeHash(input)

let verifyPassword password saltBase64 hashBase64 =
    let salt = Convert.FromBase64String(saltBase64)
    Array.concat [ salt
                   utf8Bytes password ]
    |> sha256Hash
    |> base64
    |> (=) hashBase64

let authorize (f : 'u -> UserAuthInfo -> 't) : SecureRequest<'u> -> SecureResponse<'t> =
    fun request ->
        match validateJwt request.Token with
        | None -> async { return Result.Error [AuthError TokenInvalid] }
        | Some user ->
            async {
                let output = f request.Body user
                return Result.Ok output
            }

let authorizeAsync  (f : 'u -> UserAuthInfo -> Async<'t>) : SecureRequest<'u> -> SecureResponse<'t> =
    fun request ->
        match validateJwt request.Token with
        | None -> async { return Result.Error [AuthError TokenInvalid] }
        | Some user ->
            async {
                let! output = f request.Body user
                return Result.Ok output
                }

let authorizeAny (f : UserAuthInfo -> 't) : string -> SecureResponse<'t> =
    fun token ->
        match validateJwt token with
        | None -> async { return Result.Error [AuthError TokenInvalid] }
        | Some user ->
            async {
                let output = f user
                return Result.Ok output
            }

let authorizeAnyAsync (f : UserAuthInfo -> Async<'t>) : string -> SecureResponse<'t> =
    fun token ->
        match validateJwt token with
        | None -> async { return Result.Error [AuthError TokenInvalid] }
        | Some user -> async { let! output = f user
                               return Result.Ok output }
