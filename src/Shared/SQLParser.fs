module SQLParser

#if INTERACTIVE
#load "SQLAST.fs"
#load "SQLParserLabels.fs"
#load "ParserCombinators.fs"
#load "DatabaseSchema.fs"
#load "ResultExtensions.fs"
#endif

open SQLAST
open DatabaseSchema
open ParserCombinator
open Microsoft.FSharp.Reflection
open ResultExtensions
open ProjectSpecificLabels

let unescapedChar =
    satisfy (fun c -> c <> '\\' && c <> '\"' && c <> '\'') NoLabelSpecified

let escapedChar =
    [ 
    // (stringToMatch, resultChar)
    ("\\\"",'\"')      // quote
    ("\\\\",'\\')      // reverse solidus
    ("\\/",'/')        // solidus
    ("\\b",'\b')       // backspace
    ("\\f",'\f')       // formfeed
    ("\\n",'\n')       // newline
    ("\\r",'\r')       // cr
    ("\\t",'\t')       // tab
    ("\\%", '%')       // % char, for LIKE clauses % is a wildcard
    ("\\_", '_')       // _ char, for LIKE clauses _ is a wildcard
    ] 
    // convert each pair into a parser
    |> List.map (fun (toMatch,result) -> 
        pstring toMatch |>> (fun r -> result))
    // and combine them into one
    |> choice


let stringLiteral =
    let quote = satisfy (fun c -> c = '\'' || c = '\"') NoLabelSpecified
    let allChars = unescapedChar <|> escapedChar
    quote >>. manyChars allChars .>> quote
    |>> Data.String
    <?> (CustomLabel StringLiteral)

let integerLiteral =
    let plusSignP = pchar '+'
    (opt plusSignP) >>. pint
    |>> Data.Int
    <?> Integer


let floatingPointLiteral =
    let plusSignP = pchar '+'
    (opt plusSignP) >>. pfloat
    |>> Data.Float
    <?> Float

let (>>%) p x = p |>> (fun _ -> x)

// with optional whitespace between
let (.>.) a b = a .>> (spaces) .>>. b
let (>.) a b = a .>> (spaces) >>. b
let (.>) a b = a .>> (spaces) .>> b

// with mandatory whitespace between
let (.>.>.) a b = a .>> (spaces1) .>>. b
let (>.>.) a b = a .>> (spaces1) >>. b
let (.>.>) a b = a .>> (spaces1) .>> b

let numericLiteral = floatingPointLiteral <|> integerLiteral

let dataLiteralP =
    choice [
    stringLiteral
    attemptP floatingPointLiteral
    integerLiteral
    ]
    .>> spaces

let relationOperatorP =
    let operatorPwithLabel str op =
        pstring str
        .>> spaces
        >>% op
    choice [
            operatorPwithLabel "=" Equals
            operatorPwithLabel ">=" GreaterOrEquals
            operatorPwithLabel "<=" SmallerOrEquals
            operatorPwithLabel "<>" NotEquals
            operatorPwithLabel ">" Greater
            operatorPwithLabel "<" Smaller
    ]

let andOpP =
    pstringInsensitive "and" >>% BinaryBooleanOp.And

let orOpP =
    pstringInsensitive "or" >>% BinaryBooleanOp.Or

let notOpP =
    pstringInsensitive "not"
    .>> spaces
    >>% UnaryBinaryOperator.Not

let braceP p =
    pchar '(' >. p .>  pchar ')'

let mulDivOpP =
    pchar '*' >>% BinaryNumericOp.Mul
    <|> (pchar '/' >>% BinaryNumericOp.Div)

let addSubOpP =
    pchar '+' >>% BinaryNumericOp.Add
    <|> (pchar '-' >>% Sub)

let boolP =
    pstringInsensitive "true" >>% true
    <|> (pstringInsensitive "false" >>% false)

let columnParsers<'c> (columnP : Parser<Column<'c>,BasicLabel,BasicParserError>) =
    let fieldExprP, fieldExprPRef = createParserForwardedToRef<FieldExpr<'c>,BasicParserError>()
    let termP =
        columnP |>> FieldExpr.Column
        <|> (dataLiteralP |>> FieldExpr.Value)
        <|> (braceP fieldExprP |>> FieldExpr.BracedFieldExpr)
    
    let originalTermP =  termP

    let binaryExprRebuilder exprType (firstTerm, extraTerms) =
        List.fold (fun expr (op, nextExpr) ->
            exprType (expr,op,nextExpr)) firstTerm extraTerms
    
    let mulDivP =
        termP .>.
        (mulDivOpP .>. termP |> many)
        |>> binaryExprRebuilder BinaryFieldExpr
    
    let addSubP =
        mulDivP .>.
        (addSubOpP .>. mulDivP |> many)
        |>> binaryExprRebuilder BinaryFieldExpr

    let fixRecursiveParserLabel parserRef parserRefImpl rootParser =
        parserRef := parserRefImpl
        rootParser //<?> (getLabel parserRefImpl)
    let fieldExprP = fixRecursiveParserLabel fieldExprPRef addSubP fieldExprP
    

    let boolExprP, boolExprPRef = createParserForwardedToRef<BoolExpr<'c>,BasicParserError>()
    let notBoolExpr = pstringInsensitive "not" >.>. boolExprP |>> BoolExpr.Not
    let relationExprP =
        fieldExprP .>. relationOperatorP .>. fieldExprP
        |>> (fun ((l,op),r) -> RelationExpr (l,op,r))
    let termP =
        boolP |>> BoolLiteral
        <|> (braceP boolExprP |>> BracedBoolExpr)
        <|> relationExprP
        <|> notBoolExpr
    let andExprP =
        termP .>>.
        (spaces >>. andOpP .>.>. termP |> many)
        |>> binaryExprRebuilder BinaryBoolExpr
        
    let orExprP =
        andExprP .>>.
        (spaces  >>. orOpP .>.>. andExprP |> many)
        |>> binaryExprRebuilder BinaryBoolExpr
    
    let boolExprP = fixRecursiveParserLabel boolExprPRef orExprP boolExprP

    {|FieldExprP=fieldExprP; BoolExprP=boolExprP; TermP = originalTermP|}

let columnP<'c> (getColumnName: 'c -> string) =
    FSharpType.GetUnionCases typeof<'c>
    |> Seq.collect (fun tableCase ->
        match tableCase.GetFields() with
        | [|singleFiled|] ->
            singleFiled |> (fun caseFileld ->
                FSharpType.GetUnionCases caseFileld.PropertyType
                |> Seq.map (fun columnCase ->
                    if columnCase.GetFields().Length = 0 then
                        let column = FSharpValue.MakeUnion(columnCase, Array.zeroCreate(0))
                        FSharpValue.MakeUnion(tableCase, [|column|])
                    else
                        sprintf "Column cases are expected to not have any fields! \n Could not initialize columnParser for %s column of %s table of %s database" columnCase.Name columnCase.DeclaringType.Name tableCase.DeclaringType.Name
                        |> failwith
                )
            )
        | _ ->
            sprintf "Table cases are expected to have a single field of the table discriminated union they represent in the database! \n Could not initialize columnParser for %s table of %s database" tableCase.Name tableCase.DeclaringType.Name
            |> failwith
    )
    |> Seq.cast<'c>
    |> Seq.map (fun columnCase -> 
        let colName = getColumnName columnCase
        colName
        |> pstring
        .>> spaces
        >>% {Col=columnCase;Type=DBString}
        <?> (ColumnName colName |> CustomLabel))
    |> List.ofSeq
    |> choice

let projectAppColumnP = columnP<ProjectAppColumn> getColumnName

let projectAppBoolExprP = (columnParsers projectAppColumnP)

//run projectAppBoolExprP.BoolExprP "1+1*2=3 and 3-1+2=4"
//run projectAppBoolExprP.BoolExprP "1+ 1*2=3 AND 3 - 1 + 2=4"

type ExprError =
    | InvalidOperand of BinaryNumericOp * Data
    | Errors of ExprError list
    | DivisionByZero
    | CantSolveColumnRef
    | InvalidLiteralCast of Original: Data * TargetType: Data

let addData left right =
    match left with
    | Int l ->
        match right with
        | Int r -> l + r |> Int
        | Data.Float r -> float l + r |> Data.Float
        | Data.String r -> string l + r |> Data.String
    | Data.Float l ->
        match right with
        | Int r -> l + float r |> Data.Float
        | Data.Float r -> l + r |> Data.Float
        | Data.String r -> string l + r |> Data.String
    | Data.String l ->
        match right with
        | Int r -> l + string r |> Data.String
        | Data.Float r -> l + string r |> Data.String
        | Data.String r -> l + r |> Data.String
    |> Ok
let subData left right =
    match left, right with
    | _, Data.String _ ->  InvalidOperand (Sub,right) |> Error
    | Data.String _, _ -> InvalidOperand (Sub,right) |> Error
    | Data.Int l, Int r -> l - r |> Int |> Ok
    | Data.Int l,  Data.Float r -> float l - r |> Data.Float |> Ok
    | Data.Float l, Int r -> l - float r |> Data.Float |> Ok
    | Data.Float l, Data.Float r -> l - r |> Data.Float |> Ok
let mulData left right =
    match left, right with
    | _, Data.String _ ->  InvalidOperand (Sub,right) |> Error
    | Data.String _, _ -> InvalidOperand (Sub,right) |> Error
    | Int l, Int r -> l * r |> Int |> Ok
    | Int l,  Data.Float r -> float l * r |> Data.Float |> Ok
    | Data.Float l, Int r -> l * float r |> Data.Float |> Ok
    | Data.Float l, Data.Float r -> l * r |> Data.Float |> Ok
let divData left right =
    match left, right with
    | _, Data.String _ ->  InvalidOperand (Sub,right) |> Error
    | Data.String _, _ -> InvalidOperand (Sub,right) |> Error
    | Int num, _ | _, Int num when num = 0  ->
        DivisionByZero |> Error
    | Data.Float num, _ | _, Data.Float num when num = 0.0 ->
        DivisionByZero |> Error
    | Int l, Int r -> l / r |> Int |> Ok
    | Int l,  Data.Float r -> float l / r |> Data.Float |> Ok
    | Data.Float l, Int r -> l / float r |> Data.Float |> Ok
    | Data.Float l, Data.Float r -> l / r |> Data.Float |> Ok

let solveBinaryFieldExpr left op right =
    match op with
    | Add -> addData left right
    | Sub -> subData left right
    | Mul -> mulData left right
    | Div -> divData left right

let rec binaryFieldExprSolver expr =
    match expr with
    | Value data -> data |> Ok
    //| Column col -> failwith "not implemented"
    | BracedFieldExpr expr -> binaryFieldExprSolver expr
    | BinaryFieldExpr (left,op,right) ->
        result {
            let! left = binaryFieldExprSolver left
            let! right = binaryFieldExprSolver right
            return! solveBinaryFieldExpr left op right
        }
    | Column(_) -> Error CantSolveColumnRef 

let dataCastToFloat data =
    match data with
    | Int i -> float i |> Data.Float |> Ok
    | Data.Float _ -> data |> Ok
    | Data.String s ->
        let success, parsed = System.Double.TryParse s
        match success with
        | false -> InvalidLiteralCast(data,Data.Float 0.0) |> Error
        | true -> Data.Float parsed |> Ok

let dataCastToString data =
    match data with
    | Int i -> string i
    | Data.Float f -> string f
    | Data.String s -> s
    |> Data.String

let dataCastToInt data =
    match data with
    | Int _ -> data |> Ok
    | Data.Float f -> int f |> Int |> Ok
    | Data.String s ->
        let success, parsed = System.Int32.TryParse s
        match success with
        | false -> InvalidLiteralCast(data,Data.Int 0) |> Error
        | true -> Data.Int parsed |> Ok

let dataUpCastToCommonRepresentation left right =
    match left, right with
    | Data.String s, _ -> (left, dataCastToString right) |> Ok
    | _, Data.String s -> (dataCastToString left, right) |> Ok
    | Data.Float s, _ -> dataCastToFloat right |> Result.map (fun r -> (left,r))
    | _, Data.Float s -> dataCastToFloat left  |> Result.map (fun l -> (l,right))
    | _, _ -> (left, right) |> Ok
    


let rec boolExprSolver expr =
    match expr with
    | BoolLiteral(lit) -> lit |> Ok
    | BinaryBoolExpr(l,o,r) ->
        result {
            let! left = boolExprSolver l
            let! right = boolExprSolver r
            return match o with
                    | And -> left && right
                    | Or -> left || right
        }
    | RelationExpr(l, o, r) ->
        result {
            let! l = binaryFieldExprSolver l
            let! r = binaryFieldExprSolver r
            let! l,r = dataUpCastToCommonRepresentation l r
            return match o with
                    | Equals -> l = r
                    | Greater -> l > r
                    | GreaterOrEquals -> l >= r
                    | Smaller -> l < r
                    | SmallerOrEquals -> l <= r
                    | NotEquals -> l <> r
        }
    | Not(e) -> boolExprSolver e |> Result.map not
    | BracedBoolExpr(e) -> boolExprSolver e