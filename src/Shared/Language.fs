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
        | MinLength l -> "Min length: " + string l
        | NoSpecialChar -> "Can't contain special characters!"
        | LacksNumericChar -> "Needs at least one numeric character"
        | LacksUpperChar -> "Needs at least one upper case character"
        | LacksLowerChar -> "Needs at least one lower case character"

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
        | MinLength l -> "Minimum hossz: " + string l
        | NoSpecialChar -> "Nem tartalmazhat speciális karaktert"
        | LacksNumericChar -> "Tartalmaznia kell minimum egy szám karaktert."
        | LacksUpperChar -> "Tartalmaznia kell minimum egy nagy betűt."
        | LacksLowerChar -> "Tartalmaznia kell minimum egy kis betűt."

let getMLString lang str =
    match lang with
    | English -> englishString str
    | Hungarian -> hungarianString str
