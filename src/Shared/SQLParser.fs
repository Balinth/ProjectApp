module SQLParser

#if INTERACTIVE
#load "SQLAST.fs"
#load "ParserCombinators.fs"
#load "DatabaseSchema.fs"
#load "ResultExtensions.fs"
#endif

open SQLAST
open DatabaseSchema
open ParserCombinator
open Microsoft.FSharp.Reflection
open ResultExtensions

type SQLASTLabel =
    | ColumnName of string
    | AndSoOnRecursively
    | RelationOp of RelationOp
    | BinaryBooleanOp of BinaryBooleanOp
    | UnaryBooleanOp of UnaryBinaryOperator
    | StringLiteral
    | IntegerLiteral
    | FloatingPointLiteral
    | Or of SQLASTLabel * SQLASTLabel
    | AndThen of SQLASTLabel * SQLASTLabel
    | AnyOf of SQLASTLabel list
    | ExpectedChar of char
    | Many of SQLASTLabel
    

let pchar c = pchar ExpectedChar c
let many p = many p <?> (getLabel p |> Many)
let (.>>) a b = a .>> b <?> (getLabel a)
let (>>.) a b = a >>. b <?> (getLabel b)

let unescapedChar =
    satisfy (fun c -> c <> '\\' && c <> '\"' && c <> '\'') ()

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
    let quote = satisfy (fun c -> c = '\'' || c = '\"') ()
    let allChars = unescapedChar <|> escapedChar
    quote >>. manyChars allChars .>> quote
    |>> Data.String
    <?> StringLiteral

let integerLiteral =
    let plusSignP = pchar '+'
    (opt plusSignP) >>. pint ignore
    |>> Data.Int
    <?> IntegerLiteral


let floatingPointLiteral =
    let plusSignP = pchar '+'
    (opt plusSignP) >>. pfloat ignore
    |>> Data.Float
    <?> FloatingPointLiteral


let (<|>) a b = orElseL (Or) a b
let (.>>.) a b = andThenL (fun a1 b1 -> SQLASTLabel.AndThen (a1, b1)) a b
let (>>%) p x = p |>> (fun _ -> x)

let numericLiteral = floatingPointLiteral <|> integerLiteral

let dataLiteralP = choiceL AnyOf [stringLiteral; floatingPointLiteral; integerLiteral] .>> spaces ()

let relationOperatorP =
    let operatorPwithLabel str op =
        pstring str
        <?> (op |> RelationOp)
        .>> spaces ()
        >>% op
    choiceL AnyOf [
            operatorPwithLabel "=" Equals
            operatorPwithLabel ">=" GreaterOrEquals
            operatorPwithLabel "<=" SmallerOrEquals
            operatorPwithLabel "<>" NotEquals
            operatorPwithLabel ">" Greater
            operatorPwithLabel "<" Smaller
    ]

let binaryBoolOpP =
    let operatorPwithLabel str op =
        pstringInsensitive str
        <?> (op |> BinaryBooleanOp)
        .>> spaces ()
        >>% op
    choiceL AnyOf [
        operatorPwithLabel "AND" BinaryBooleanOp.And
        operatorPwithLabel "OR" BinaryBooleanOp.Or
    ]

let notOpP =
    pstringInsensitive "not"
    <?> (UnaryBinaryOperator.Not |> UnaryBooleanOp)
    .>> spaces ()
    >>% UnaryBinaryOperator.Not

let (.>.) a b = a .>> (spaces ()) .>>. b
let (>.) a b = a .>> (spaces ()) >>. b
let (.>) a b = a .>> (spaces ()) .>> b

let braceP p =
    pchar '(' >. p .>  pchar ')'

let mulDivOpP =
    pchar '*' >>% BinaryNumericOp.Mul
    <|> (pchar '/' >>% BinaryNumericOp.Div)

let addSubOpP =
    pchar '+' >>% BinaryNumericOp.Add
    <|> (pchar '-' >>% Sub)

let columnParsers<'c> (columnP : Parser<Column<'c>,SQLASTLabel,BasicParserError>) =
    let fieldExprP, fieldExprPRef = createParserForwardedToRef<FieldExpr<'c>,BasicParserError>()
    let fieldExprP = fieldExprP <?> AndSoOnRecursively
    let termP =
        columnP |>> FieldExpr.Column
        <|> (dataLiteralP |>> FieldExpr.Value)
        <|> (braceP fieldExprP)
    
    let binaryExprRebuilder (firstTerm, extraTerms) =
        List.fold (fun expr (op, nextExpr) ->
            BinaryFieldExpr (expr,op,nextExpr)) firstTerm extraTerms
    
    let mulDivP =
        termP .>.
        (mulDivOpP .>. termP |> many)
        |>> binaryExprRebuilder
    
    let addSubP =
        mulDivP .>.
        (addSubOpP .>. mulDivP |> many)
        |>> binaryExprRebuilder

    fieldExprPRef := addSubP <?> ()
    fieldExprP
    //let boolExprP, boolExprPRef = createParserForwardedToRef<BoolExpression<'c>,BasicParserError>()
    //let boolExprP = boolExprP <?> AndSoOnRecursively
    //let binaryBoolExprP =
    //    boolExprP .>>. binaryBoolOpP .>>. boolExprP
    //    |>> (fun (((leftExpr),op),(rightExpr)) -> BinaryExpr(leftExpr,op,rightExpr))
    //let relationExprP =
    //    fieldExpressionP .>>. relationOperatorP .>>. fieldExpressionP
    //    |>> (fun (((leftExpr),op),(rightExpr)) -> RelationExpr(leftExpr,op,rightExpr))
    //let unaryNotExprP =
    //    notOpP >>. boolExprP |>> BoolExpression.Not
//
    //let fixRecursiveParserDef parserList =
    //    boolExprPRef := choice parserList
    //    boolExprP <?> (List.map getLabel parserList |> AnyOf)
    //    
//
    //let boolExprP = fixRecursiveParserDef [
    //    relationExprP
    //    unaryNotExprP
    //    binaryBoolExprP
    //    ]
    //{|BoolExprP=boolExprP|}

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
        .>> spaces ()
        >>% {Col=columnCase;Type=DBString}
        <?> ColumnName colName)
    |> List.ofSeq
    |> choiceL AnyOf

let projectAppColumnP = columnP<ProjectAppColumn> getColumnName

let projectAppBoolExprP = (columnParsers projectAppColumnP)

type ExprError =
    | InvalidOperand of BinaryNumericOp * Data
    | DivisionByZero

let addData left right =
    match left with
    | Int l ->
        match right with
        | Int r -> l + r |> Int
        | Float r -> float l + r |> Float
        | String r -> string l + r |> String
    | Float l ->
        match right with
        | Int r -> l + float r |> Float
        | Float r -> l + r |> Float
        | String r -> string l + r |> String
    | String l ->
        match right with
        | Int r -> l + string r |> String
        | Float r -> l + string r |> String
        | String r -> l + r |> String
    |> Ok
let subData left right =
    match left, right with
    | _, String _ ->  InvalidOperand (Sub,right) |> Error
    | String _, _ -> InvalidOperand (Sub,right) |> Error
    | Int l, Int r -> l - r |> Int |> Ok
    | Int l,  Float r -> float l - r |> Float |> Ok
    | Float l, Int r -> l - float r |> Float |> Ok
    | Float l, Float r -> l - r |> Float |> Ok
let mulData left right =
    match left, right with
    | _, String _ ->  InvalidOperand (Sub,right) |> Error
    | String _, _ -> InvalidOperand (Sub,right) |> Error
    | Int l, Int r -> l * r |> Int |> Ok
    | Int l,  Float r -> float l * r |> Float |> Ok
    | Float l, Int r -> l * float r |> Float |> Ok
    | Float l, Float r -> l * r |> Float |> Ok
let divData left right =
    match left, right with
    | _, String _ ->  InvalidOperand (Sub,right) |> Error
    | String _, _ -> InvalidOperand (Sub,right) |> Error
    | Int num, _ | _, Int num when num = 0  ->
        DivisionByZero |> Error
    | Float num, _ | _, Float num when num = 0.0 ->
        DivisionByZero |> Error
    | Int l, Int r -> l / r |> Int |> Ok
    | Int l,  Float r -> float l / r |> Float |> Ok
    | Float l, Int r -> l / float r |> Float |> Ok
    | Float l, Float r -> l / r |> Float |> Ok

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

run (projectAppBoolExprP |>> binaryFieldExprSolver) "1+1-1"
run (projectAppBoolExprP |>> binaryFieldExprSolver) "-1+1-1"
run (projectAppBoolExprP |>> binaryFieldExprSolver) "1-2-3"


let testPrinter = printResult (fun a -> "") (fun e -> e.ToString())
run escapedChar "g\"" |> testPrinter

run booleanOperator "oR"
run stringLiteral "\"ab\\tde\""
run integerLiteral "-12121"
run floatingPointLiteral "+1111:0"
run dataLiteralP "\"fisfos\""
run dataLiteralP "111"
run dataLiteralP "111."
run dataLiteralP "fdsfs"
run dataLiteralP "\"fsfds" // should not work...
