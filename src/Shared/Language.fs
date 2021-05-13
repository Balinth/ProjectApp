module Language

open Validation
open Shared
open SQLAST
open SQLGenerator
open DatabaseSchema

type Language =
    | English
    | Hungarian

type LStr =
    | ErrorNotLoggedIn
    | Account
    | Register
    | Login
    | Logout
    | ApplicationName
    | Username
    | Password
    | PasswordConfirm
    | GivenName
    | FamilyName
    | Email
    | InvalidEmail
    | User
    | Query
    | ValidationError of Validation.ValidationError
    | ClientError of ClientError
    | APIError of APIError
    | LoginError of LoginError
    | RegistrationError of RegistrationError

let foldListNewLines =
    List.fold (fun state str -> state + "\n" + str) ""

let rec englishString mlString =
    match mlString with
    | ErrorNotLoggedIn -> "Error: not logged in."
    | Account -> "Account"
    | Register -> "Register"
    | Login -> "Login"
    | Logout -> "Logout"
    | ApplicationName -> "BIM-BAM"
    | Username -> "Username"
    | Password -> "Password"
    | PasswordConfirm -> "Password confirmation"
    | User -> "User"
    | GivenName -> "Given name"
    | FamilyName -> "Family name"
    | Email -> "Email"
    | InvalidEmail -> "Invalid email"
    | Query -> "Query"
    | ValidationError err ->
        match err with
        | NoWhitespace -> "No whitespaces!"
        | MinLength l -> "Min length: " + string l
        | NoSpecialChar -> "Can't contain special characters!"
        | LacksNumericChar -> "Needs at least one numeric character."
        | LacksUpperChar -> "Needs at least one upper case character."
        | LacksLowerChar -> "Needs at least one lower case character."
        | PasswordConfirmationDoesNotMatch -> "Password confirmation does not match."

    | ClientError clientErr ->
        match clientErr with
        | UserInfoWithoutToken -> "Cant understand user info without auth token."

    | APIError apiError ->
        match apiError with
        | AuthError authErr ->
            match authErr with
            | TokenInvalid -> "Invalid auth token"
            | UserUnauthorized -> "Unauthorized user"
        | DBError dbErr ->
            match dbErr with
            | DBException ex -> sprintf "Exception: %A" ex
            | SQLError sqlErr ->
                match sqlErr with
                | QueryHasNoColumns -> "Query has no columns"
                | SyntaxError -> "Syntax error"
                | OperatorMustHaveArguments op -> sprintf "Operator must have arguments: %s" (stringizeBoolOperator op)
                | AllColumnsMustBeFromTable(expectedTable, externalColumns) ->
                    let tableName = getTableName expectedTable
                    let externalColumns =
                        externalColumns
                        |> List.map getColumnName
                        |> List.fold (fun state str -> state + "\n" + str) ""
                    sprintf "All columns must be from the same table.\nExpected table: %s, External columns:\n%s" tableName externalColumns
                | InsertMustHaveColumns -> "Insert statement must have columns."
                | InsertMustContainDistinctColumns(nonDistinct) ->
                    let nonDistinctCols =
                        nonDistinct
                        |> List.map getColumnName
                        |> List.fold (fun state str -> state + "\n" + str) ""
                    sprintf "All columns must be distinct. Non distinct colums:\n%s" nonDistinctCols
            | InsertFailed -> "Insert failed."
            | MoreThanOneResult -> "More than one result."
            | MissingData -> "Missing data"

    | LoginError loginError ->
        match loginError with
        | PasswordIncorrect -> "Incorrect password."
        | UsernameDoesNotExist -> "Username does not exist."
        | UnexpectedLoginError(ex) -> sprintf "Exception: %A" ex

    | RegistrationError registrationError ->
        match registrationError with
        | UserNameTaken -> "User name already taken."
        | RegistrationError.ValidationError(passwordProblem, userNameProblem) ->
            let passwordErrors =
                passwordProblem
                |> List.map (ValidationError >> englishString)
                |> foldListNewLines
            let usernameErrors =
                userNameProblem
                |> List.map (ValidationError >> englishString)
                |> foldListNewLines
            sprintf "Password problems:\n%s\nUsername problems:\n%s\n" passwordErrors usernameErrors
        | RegistrationError.APIError apiErros ->
            let errors =
                apiErros
                |> List.map (APIError >> englishString)
                |> foldListNewLines
            sprintf "API Errors:\n%s" errors
        | UnexpectedRegistrationError err -> "Unexpected error: " + err


let rec hungarianString mlString =
    match mlString with
    | ErrorNotLoggedIn -> "Hiba: nincs bejelentkezve."
    | Account -> "Felhasználói fiók"
    | Register -> "Regisztráció"
    | Login -> "Bejelentkezés"
    | Logout -> "Kijelentkezés"
    | ApplicationName -> "BIM-BAM"
    | Username -> "Felhasználó név"
    | Password -> "Jelszó"
    | PasswordConfirm -> "Jelszó megerősítés"
    | User -> "Felhasználó"
    | GivenName -> "Keresztnév"
    | FamilyName -> "Vezetéknév"
    | Email -> "Email"
    | InvalidEmail -> "Hibás email"
    | Query -> "Lekérdezés"
    | ValidationError err ->
        match err with
        | NoWhitespace -> "Nem lehet szóköz!"
        | MinLength l -> "Minimum hossz: " + string l
        | NoSpecialChar -> "Nem tartalmazhat speciális karaktert"
        | LacksNumericChar -> "Tartalmaznia kell minimum egy szám karaktert."
        | LacksUpperChar -> "Tartalmaznia kell minimum egy nagy betűt."
        | LacksLowerChar -> "Tartalmaznia kell minimum egy kis betűt."
        | PasswordConfirmationDoesNotMatch -> "A jelszó megerősítés nem egyezik."

    | ClientError clientErr ->
        match clientErr with
        | UserInfoWithoutToken -> "Nem értelmezhető felhasználói információ auth token nélkül."

    | APIError apiError ->
        match apiError with
        | AuthError authErr ->
            match authErr with
            | TokenInvalid -> "Érvénytelen auth token"
            | UserUnauthorized -> "Illetéktelen hozzáférés"
        | DBError dbErr ->
            match dbErr with
            | DBException ex -> sprintf "Kivétel: %A" ex
            | SQLError sqlErr ->
                match sqlErr with
                | QueryHasNoColumns -> "A Lekérés nem tartalmaz oszlopokat."
                | SyntaxError -> "Szintaktikai hiba."
                | OperatorMustHaveArguments op -> sprintf "Hiányzó operátor argumentumok: %s" (stringizeBoolOperator op)
                | AllColumnsMustBeFromTable(expectedTable, externalColumns) ->
                    let tableName = getTableName expectedTable
                    let externalColumns =
                        externalColumns
                        |> List.map getColumnName
                        |> List.fold (fun state str -> state + "\n" + str) ""
                    sprintf "Nem minden oszlop tartozik ugyan azon táblához.\nTábla: %s, Idegen oszlopok:\n%s" tableName externalColumns
                | InsertMustHaveColumns -> "Beszúró parancsnak tartalmaznia kell legalább egy oszlopot."
                | InsertMustContainDistinctColumns(nonDistinct) ->
                    let nonDistinctCols =
                        nonDistinct
                        |> List.map getColumnName
                        |> List.fold (fun state str -> state + "\n" + str) ""
                    sprintf "Minden oszlopnak egyedinek kell lennie. Nem egyedi oszlopok:\n%s" nonDistinctCols
            | InsertFailed -> "Beszúrás sikertelen."
            | MoreThanOneResult -> "Több mint egy eredmény."
            | MissingData -> "Hiányzó adat"

    | LoginError loginError ->
        match loginError with
        | PasswordIncorrect -> "Helytelen jelszó."
        | UsernameDoesNotExist -> "Nem létezik ilyen nevű felhasználó."
        | UnexpectedLoginError(ex) -> sprintf "Kivétel: %A" ex

    | RegistrationError registrationError ->
        match registrationError with
        | UserNameTaken -> "Ez a felhasználó név már foglalt."
        | RegistrationError.ValidationError(passwordProblem, userNameProblem) ->
            let passwordErrors =
                passwordProblem
                |> List.map (ValidationError >> hungarianString)
                |> foldListNewLines
            let usernameErrors =
                userNameProblem
                |> List.map (ValidationError >> hungarianString)
                |> foldListNewLines
            sprintf "Jelszó problémák:\n%s\nFelhasználó név problémák:\n%s\n" passwordErrors usernameErrors
        | RegistrationError.APIError apiErros ->
            let errors =
                apiErros
                |> List.map (APIError >> hungarianString)
                |> foldListNewLines
            sprintf "API Hibák:\n%s" errors
        | UnexpectedRegistrationError err -> "Váratlan hiba: " + err

let getMLString lang str =
    match lang with
    | English -> englishString str
    | Hungarian -> hungarianString str
