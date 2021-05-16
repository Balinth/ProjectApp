module ParserLanguageEng

open ProjectSpecificLabels
open ParserCombinator

let rec projectSpecificLabelEnglish label =
    match label with
    | BinaryExpr -> "Binary expression"
    | NumericExpr -> "Numeric expression"
    | StringLiteral -> "String literal"
    | ColumnName c -> "Column name:" + c
    | TableName t -> "Table name:" + t
    | DatabaseColumnName -> "Column name"
    | DatabaseTableName -> "Table name"
    | SelectStatement -> "Select statement"
    | WhereExpression -> "Where expression"

let rec labelEnglish indent label =
    let indentation =
        [0..indent]
        |> List.fold (fun s _ -> s + "\t") ""
    let labelStr =
        match label with
        | EndOfInput -> "End of input"
        | NoLabelSpecified -> "No label"
        | Recursive -> "recursive.."
        | Attempt l -> sprintf "Attempt:%s" (labelEnglish indent l)
        | Sequence l ->
            match l with
            | head::rest ->
                rest
                |> List.fold (fun s i -> s + ";\r\n" + indentation + (labelEnglish (indent + 1) i) ) ("Sequence:\r\n" + indentation + labelEnglish (indent + 1) head)
            | [] -> "Empty sequence"
        | AndThen(a, b) -> sprintf "%s then %s" (labelEnglish indent a) (labelEnglish indent b)
        | OrElse(a, b) ->  sprintf "%s or %s" (labelEnglish indent a) (labelEnglish indent b)
        | Choice l ->
            match l with
            | head::rest ->
                rest
                |> List.fold (fun s i -> s + "; " + (labelEnglish indent i) ) ("Choice: " + labelEnglish indent head)
            | [] -> "Empty choice"
        | Many p -> "One or more of: " + labelEnglish indent p
        | Optional p -> "Optional: " + labelEnglish indent p
        | String s -> sprintf "String: \"%s\"" s
        | CaseInsensitiveString s -> sprintf "Case insensitive string: \"%s\"" s
        | Char c -> sprintf "Char:'%s'" c
        | Integer -> "Integer"
        | DigitChar -> "Digit"
        | Float -> "Decimal number"
        | EmptySequence -> "Empty sequence"
        | WhiteSpace -> "White space"
        | Inside(parent, local) -> sprintf "%s while inside %s" (labelEnglish indent local) (labelEnglish (indent + 1) parent)
        | CustomLabel l -> projectSpecificLabelEnglish l
    labelStr

let rec projectSpecificErrorEnglish e =
    match e with
    | SQLASTError e -> "SQLAST Error"
    | SelectedColumnsTablesMismatch -> "Selected columns do not match source tables!"

let rec parserErrorEnglish e =
    match e with
    | NoMoreInput -> "No more input"
    | UnexpectedChar c -> sprintf "Unexpected character: '%s'" c
    | Int32Overflow d -> sprintf "Integer overflow while trying to parse %s into a 32 bit integer" d
    | Float64Overflow f -> sprintf "Float 64 overflow while trying to parse %s into a 64 bit floating point number" f
    | NonFatal e -> "Non fatal error: " + (parserErrorEnglish e)
    | CustomError c -> projectSpecificErrorEnglish c

let printResultEng labelPrinter errorPrinter result =
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
        sprintf "Line:%i Col:%i \n%s\n%s Error parsing: %s"
            linePos colPos errorLine failureCaret label

let printProjectSpecificResultEnglish (r:ParserResult<'a,BasicLabel,BasicParserError>) =
    printResultEng (simplifyAndThensLabel >> labelEnglish 0) parserErrorEnglish r
