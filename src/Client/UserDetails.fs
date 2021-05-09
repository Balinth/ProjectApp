module UserDetails

open Shared
open Language
open Fable.React.Helpers

// note: general user handling (eg login logout, auth token handling are handled in the root component as cross cutting concerns)

let view lstr (user:UserInfo option) dispatch =
    match user with
    | None -> lstr LStr.ErrorNotLoggedIn |> str
    | Some user -> str user.UserName
