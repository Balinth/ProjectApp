module Validation

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
    | NotEmpty
    | MinLength of int
let validateNoWhitespace s =
    match s with
    | (s:string) when s.Contains(" ") || s.Contains("\t") || s.Contains("\v")|| s.Contains("\n")|| s.Contains("\r") ->
        Error [NoWhitespace]
    | _ -> Ok s
let validateNonZeroLength (s:string) =
    match s with
    | s when s.Length = 0 -> Error [NotEmpty]
    | _ -> Ok s
let validateMinLength l (s:string) =
    match s with
    | s when s.Length < l -> Error [MinLength l]
    | _ -> Ok s
let validateUsername s =
    s 
    |>( validateNonZeroLength
    &&& validateNoWhitespace)
let validatePassword s =
    s
    |>( validateMinLength 8
    &&& validateNoWhitespace)