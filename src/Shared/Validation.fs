module Validation

open System
open ResultExtensions

let plus addSuccess addFailure switch1 switch2 x =
    match (switch1 x),(switch2 x) with
    | Ok s1,Ok s2 -> Ok (addSuccess s1 s2)
    | Error f1,Ok _  -> Error f1
    | Ok _ ,Error f2 -> Error f2
    | Error f1,Error f2 -> Error (addFailure f1 f2)
let (&&&) a b =
    let addSuccess r1 r2 = r1
    let addFailure r1 r2 = List.append r1 r2
    plus addSuccess addFailure a b

type ValidationError =
    | NoWhitespace
    | NoSpecialChar
    | MinLength of int
    | LacksNumericChar
    | LacksUpperChar
    | LacksLowerChar
    | PasswordConfirmationDoesNotMatch

let validateNoWhitespace s =
    match s with
    | (s:string) when s.Contains(" ") || s.Contains("\t") || s.Contains("\v")|| s.Contains("\n")|| s.Contains("\r") ->
        Error [NoWhitespace]
    | _ -> Ok s
let validateMinLength l (s:string) =
    match s with
    | s when s.Length < l -> Error [MinLength l]
    | _ -> Ok s
let validateHasNumericChar s =
    match s with
    | (s:string) when s.ToCharArray() |> Seq.exists Char.IsDigit -> Ok s
    | _ -> Error [LacksNumericChar]
let validateHasUpperChar s =
    match s with
    | (s:string) when s.ToCharArray() |> Seq.exists Char.IsUpper -> Ok s
    | _ -> Error [LacksUpperChar]
let validateHasLowerChar s =
    match s with
    | (s:string) when s.ToCharArray() |> Seq.exists Char.IsLower -> Ok s
    | _ -> Error [LacksLowerChar]
let validateNoSpecialChar s =
    match s with
    | (s:string) when s.ToCharArray() |> Seq.exists (fun c -> Char.IsLetterOrDigit c |> not) -> Error [NoSpecialChar]
    | _ -> Ok s


let validateUsername s =
    let minUsernameLength = 4
    s
    |>( validateMinLength minUsernameLength
    &&& validateNoWhitespace
    &&& validateNoSpecialChar)

let validatePassword s =
    let minPwLength = 8
    s
    |>( validateMinLength minPwLength
    &&& validateNoWhitespace
    &&& validateHasNumericChar
    &&& validateHasUpperChar
    &&& validateHasLowerChar)

let checkEmail email =
    System.Text.RegularExpressions.Regex.IsMatch(email, @"^[^@\s]+@[^@\s]+\.[^@\s]+$")

let checkUsernameAndPassword userName password =
    validateUsername userName |> Result.getError [],
    validatePassword password |> Result.getError []

let checkRegistrationFields userName password email =
    validateUsername userName |> Result.getError [],
    validatePassword password |> Result.getError [],
    checkEmail email

let checkRegisterInputs userName password email passwordconfirm =
    validateUsername userName |> Result.getError [],
    validatePassword password |> Result.getError [],
    checkEmail email,
    password = passwordconfirm