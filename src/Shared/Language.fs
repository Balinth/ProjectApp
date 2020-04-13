module Language

open Validation
open Shared

type Language =
    | English
    | Hungarian

type LStr =
    | ErrorNotLoggedIn
    | Account
    | Login
    | Logout
    | ApplicationName
    | Username
    | Password
    | User
    | ValidationError of Validation.ValidationError

let englishString mlString =
    match mlString with
    | ErrorNotLoggedIn -> "Error: not logged in."
    | Account -> "Account"
    | Login -> "Login"
    | Logout -> "Logout"
    | ApplicationName -> "BIM-BAM"
    | Username -> "Username"
    | Password -> "Password"
    | User -> "User"
    | ValidationError err ->
        match err with
        | NoWhitespace -> "No whitespaces!"
        | NotEmpty -> "Not empty!"
        | MinLength l -> "Min length: " + string l

let hungarianString mlString =
    match mlString with
    | ErrorNotLoggedIn -> "Hiba: nincs bejelentkezve."
    | Account -> "Felhasználói fiók"
    | Login -> "Bejelentkezés"
    | Logout -> "Kijelentkezés"
    | ApplicationName -> "BÍM-BÁM"
    | Username -> "felhasználó név"
    | Password -> "jelszó"
    | User -> "Felhasználó"
    | ValidationError err ->
        match err with
        | NoWhitespace -> "Nem lehet szóköz!"
        | NotEmpty -> "Nem lehet üres!"
        | MinLength l -> "Minimum hossz: " + string l

let getMLString lang str =
    match lang with
    | English -> englishString str
    | Hungarian -> hungarianString str
