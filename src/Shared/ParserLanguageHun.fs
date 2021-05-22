module ParserLanguageHun

open ProjectSpecificLabels
open ParserCombinator

let printResultHun labelPrinter errorPrinter result =
    match result with
    | Ok (value,input) ->
        sprintf "%A" value
    | Error (label,error,parserPos) ->
        let label = labelPrinter label
        let error = errorPrinter error
        let errorLine = parserPos.CurrentLine
        let colPos = parserPos.Column
        let linePos = parserPos.Line
        let spaces = [1.. colPos] |> List.fold (fun s i -> s + " ") ""
        let failureCaret = sprintf "%s^%s" spaces error
        // examples of formatting
        //   sprintf "%*s^%s" 0 "" "test"
        //   sprintf "%*s^%s" 10 "" "test"
        sprintf "Sor:%i Oszlop:%i \n%s\n%s Hiba elemzés közben: %s"
            linePos colPos errorLine failureCaret label

let rec projectSpecificLabelHungarian label =
    match label with
    | BinaryExpr -> "Bináris kifejezés"
    | NumericExpr -> "Számtani kifejezés"
    | StringLiteral -> "String literális"
    | ColumnName c -> "Oszlop név:" + c
    | TableName t -> "Tábla név:" + t
    | DatabaseColumnName -> "Oszlop név"
    | DatabaseTableName -> "Tábla név"
    | SelectStatement -> "Select parancs"
    | WhereExpression -> "Where feltétel"

let rec labelHungarian indent label =
    let indentation =
        [0..indent]
        |> List.fold (fun s _ -> s + "\t") ""
    let labelStr =
        match label with
        | EndOfInput -> "Bemenet vége"
        | NoLabelSpecified -> "Nincs cimke"
        | Recursive -> "rekurzív.."
        | Attempt l -> sprintf "Próba:%s" (labelHungarian indent l)
        | Sequence l ->
            match l with
            | head::rest ->
                rest
                |> List.fold (fun s i -> s + ";\r\n" + indentation + (labelHungarian (indent + 1) i) ) ("Sorban:\r\n" + indentation + labelHungarian (indent + 1) head)
            | [] -> "Elemek nélküli sor"
        | AndThen(a, b) -> sprintf "%s és utána %s" (labelHungarian indent a) (labelHungarian indent b)
        | OrElse(a, b) ->  sprintf "%s vagy %s" (labelHungarian indent a) (labelHungarian indent b)
        | Choice l ->
            match l with
            | head::rest ->
                rest
                |> List.fold (fun s i -> s + "; " + (labelHungarian indent i) ) ("Bármelyik: " + labelHungarian indent head)
            | [] -> "Bármelyik 0 lehetőség közül"
        | Many p -> "Több: " + labelHungarian indent p
        | Optional p -> "Opcionális: " + labelHungarian indent p
        | String s -> sprintf "String: \"%s\"" s
        | CaseInsensitiveString s -> sprintf "Kis vagy nagybetűs string: \"%s\"" s
        | Char c -> sprintf "Karakter:'%s'" c
        | Integer -> "Egész szám"
        | DigitChar -> "Számjegy"
        | Float -> "Tizedes szám"
        | EmptySequence -> "Üres sorozat"
        | WhiteSpace -> "szóköz"
        | Inside(parent, local) -> sprintf "%s a következőn belül: %s" (labelHungarian indent local) (labelHungarian (indent + 1) parent)
        | CustomLabel l -> projectSpecificLabelHungarian l
    labelStr

let rec projectSpecificErrorHungarian e =
    match e with
    | SQLASTError e -> "SQLAST hiba"
    | SelectedColumnsTablesMismatch -> "A kiválasztani kívánt oszlopok nem illenek össze a forrás táblákkal!"

let rec parserErrorHungarian e =
    match e with
    | NoMoreInput -> "Nincs több input."
    | UnexpectedChar c -> sprintf "Váratlan karakter: '%s'" c
    | Int32Overflow d -> sprintf "Egész szám túlcsordulás %s 32 bites egész számba olvasása közben." d
    | Float64Overflow f -> sprintf "Lebegőpontos túlcsordulás %s 64 bites lebegőpontos számba olvasása közben." f
    | NonFatal e -> "Nem fatális hiba: " + (parserErrorHungarian e)
    | CustomError c -> projectSpecificErrorHungarian c

let printProjectSpecificResultHungarian (r:ParserResult<'a,BasicLabel,BasicParserError>) =
    printResultHun (simplifyAndThensLabel >> labelHungarian 0) parserErrorHungarian r

(*

let a = pchar 'a'
let b = pchar 'b'
let c = pchar 'c'
let d = pchar 'd'

let abc = a .>>. b .>>. c .>>. d

run abc "abd"
|> printProjectSpecificResultHungarian

let err1 =
    """"  "Sor:0 Oszlop:2
    abd
      ^Váratlan karakter: 'd' Hiba elemzés közben: Karakter:'a' és utána Karakter:'b' és utána Karakter:'c' és utána Karakter:'d'"""

let err2 =
    """"  "Sor:0 Oszlop:2
abd
  ^Váratlan karakter: 'd' Hiba elemzés közben: Sorban:
        Karakter:'a';
        Karakter:'b';
        Karakter:'c';
        Karakter:'d'" """


type Point = {
    X: int
    Y: int
}

let createPoint (x, y) = {X = x; Y = y}
let onlyXPoint (x, y) =
    if y = 0 then
        returnP NoLabelSpecified (x,y)
    else
        failP (UnexpectedChar (string y)) <?> NoLabelSpecified
let xP = pint
let yP = pint
let pointP =
    xP .>>. yP
    >>= onlyXPoint
    |>> createPoint

*)