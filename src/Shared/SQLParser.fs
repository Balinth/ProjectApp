module SQLParser

#if INTERACTIVE
#load "ResultExtensions.fs"
#load "SQLAST.fs"
#load "SQLParserLabels.fs"
#load "ParserCombinators.fs"
#load "DatabaseSchema.fs"
#endif

open ResultExtensions
open SQLAST
open DatabaseSchema
open ParserCombinator
open ProjectSpecificLabels
open Microsoft.FSharp.Reflection

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

let expressionParsers<'c> (columnP : Parser<Column<'c>,BasicLabel,BasicParserError>) =
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

    {|FieldExprP=fieldExprP; BoolExprP=boolExprP|}

let getDatabaseColumnCases<'c>() =
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
                        sprintf
                            "Column cases are expected to not have any fields!
                            Could not initialize columnParser for %s column of %s table of %s database"
                            columnCase.Name
                            columnCase.DeclaringType.Name
                            tableCase.DeclaringType.Name
                        |> failwith
                )
            )
        | _ ->
            sprintf
                "Table cases are expected to have a single field of the table discriminated union they represent in the database!
                Could not initialize columnParser for %s table of %s database"
                tableCase.Name
                tableCase.DeclaringType.Name
            |> failwith
    )
    |> Seq.cast<'c>

let getDatabaseColumns<'c> getColumnType =
    getDatabaseColumnCases<'c>()
    |> List.ofSeq
    |> List.map (fun columnCase ->
        {Col=columnCase;Type=getColumnType columnCase})

type DatabaseParser<'c,'t,'parserLabel,'parserError> = {
    ColumnNameP : Parser<Column<'c>,'parserLabel,'parserError>
    TableSpecificColumnNameP : 't -> Parser<Column<'c>,'parserLabel,'parserError>
    TableNameP : Parser<'t,'parserLabel,'parserError>
    DatabaseSchema : DatabaseSchema<'c,'t>
}

let databaseP<'c,'t when 'c : comparison> (db:DatabaseSchema<'c,'t>) =
    let allCases = getDatabaseColumnCases<'c>() |> Seq.sortDescending
    let tablePMap =
        allCases
        |> Seq.map (fun (columnCase:'c) ->
            let tableName = db.GetTableNameByColumn columnCase
            tableName
            |> pstring
            .>> spaces
            >>% db.GetColumnTable columnCase
            <?> (TableName tableName |> CustomLabel)
            |> (fun p -> columnCase, p))
        |> Map.ofSeq
    let tableP =
        tablePMap
        |> Map.toList
        |> List.distinctBy (fst >> db.GetTableNameByColumn)
        |> List.sortByDescending (fst >> db.GetTableNameByColumn)
        |> List.map snd
        |> choice
        <?> (CustomLabel DatabaseTableName)
    //let dbSpecificColumnP =

    let columnParsers cases =
        cases
        |> Seq.map (fun columnCase ->
            let colTableP = opt (tablePMap.[columnCase] .>. pchar '.' |> attemptP)
            let colName = db.GetColumnName columnCase
            (colTableP<?> (CustomLabel DatabaseTableName))
            >. pstring colName
            .>> spaces
            >>% db.GetColumn columnCase
            <?> (ColumnName colName |> CustomLabel))
        |> List.ofSeq
        |> choice

    let tableSpecificColumnP table =
        allCases
        |> Seq.filter (fun (columnCase:'c) -> db.GetTableName table = db.GetTableNameByColumn columnCase)
        |> columnParsers

    let columnP = columnParsers allCases

    {ColumnNameP=columnP;TableNameP=tableP;DatabaseSchema=db; TableSpecificColumnNameP=tableSpecificColumnP}

type PreParsedCol =
    | Prefixed of table:string * col:string
    | Naked of col:string

type SelectedCols =
    | All // *
    | OneOrMoreCols of PreParsedCol list

let identifierP =
    manyChars1 (satisfy (fun c -> System.Char.IsLetter c || c = '_') NoLabelSpecified)
    .>>. manyChars (satisfy (fun c -> System.Char.IsLetterOrDigit c || c = '_') NoLabelSpecified)
    |>> (fun (a,b) -> a + b)
    <?> (CustomLabel DatabaseColumnName)

let selectColPreP =
    pchar '*' >>% SelectedCols.All
    <|> (
        attemptP (
            identifierP .> (pchar '.') .>. identifierP .>> spaces
            |>> (fun (t,c) -> Prefixed(t,c))
            )
        <|> (identifierP .>> spaces |>> Naked)
        |> sepBy1 <| (pchar ',' .>>. spaces)
        |>> OneOrMoreCols
    )

let solveConcreteTableColumn (dbParser:DatabaseParser<'c,'t,_,_>) tables column : Column<'c> option =
    match column with
    | Prefixed (table, col) ->
        match run dbParser.TableNameP table with
        | Ok (table, input) ->
            match run (dbParser.TableSpecificColumnNameP table) col with
            | Ok (column, input) -> Some column
            | Error _ -> None
        | Error _ -> None
    | Naked col ->
        match run dbParser.ColumnNameP col with
        | Ok (col, input) ->
            match List.filter (fun t -> t = dbParser.DatabaseSchema.GetColumnTable col.Col) tables with
            | [exactlyOne] -> Some col
            | _ -> None
        | Error _ -> None


let solveSelectedColumns (dbParser:DatabaseParser<'c,'t,_,_>) tables selectedColumns : Column<'c> list option =
    match selectedColumns with
    | All ->
        getDatabaseColumns<'c> dbParser.DatabaseSchema.GetColumnType
        |> List.filter (fun c -> List.contains (dbParser.DatabaseSchema.GetColumnTable c.Col) tables)
        |> Some
    | OneOrMoreCols cols ->
        let cols = List.map (solveConcreteTableColumn dbParser tables) cols
        match List.choose id cols with
        | allOk when allOk.Length = cols.Length -> Some allOk
        | _ -> None

let selectedColumnsP dbParser =
    pchar '*' >>% (getDatabaseColumns<'c> dbParser.DatabaseSchema.GetColumnType |> List.ofSeq)
    <|> (sepBy ((dbParser.ColumnNameP) .>> spaces) (pchar ',' .>> spaces) )

let queryP databaseParser =
    let exprPs = expressionParsers (databaseParser.ColumnNameP)
    let boolExprP = exprPs.BoolExprP
    pstringInsensitive "SELECT"
    >. selectColPreP //selectedColumnsP databaseParser
    <?> (CustomLabel SelectStatement)
    .> pstringInsensitive "FROM"
    .>. (sepBy (databaseParser.TableNameP <?> (CustomLabel DatabaseTableName) .>> spaces) (pchar ',' .>> spaces))
    >>= ( fun (columns, tables) ->
        match solveSelectedColumns databaseParser tables columns with
        | Some columns ->
            returnP (DatabaseTableName |> CustomLabel) (columns, tables)
        | None ->
            failP (SelectedColumnsTablesMismatch |> CustomError)
            <?> (DatabaseTableName |> CustomLabel)
    )
    .>. opt (pstringInsensitive "WHERE"
        >. boolExprP <?> (CustomLabel WhereExpression))
    |>> (fun ((columns, tables), condition) ->
        {Columns = columns; Condition = condition}
    )
    .>> spaces .>> endOfInputP


// dbSpecificASTError: in this project will be always ProjectSpecificError.SQLASTError
// note: this is not specifically "bad" design, but is not elegant either
// the root cause of this inelegance is that the AST error should not need to know about the actual db schema.
let insertP dbSpecificASTError databaseParser =
    let dbSchema = databaseParser.DatabaseSchema
    let columnListP =
        sepBy1 (databaseParser.ColumnNameP ) (pchar ',' .>> spaces)
    let valueListP =
        sepBy1 dataLiteralP (pchar ',' .>> spaces)
    pstringInsensitive "insert into"
    >.>. databaseParser.TableNameP
    |> bindP (fun table ->
        braceP (
            sepBy1 (databaseParser.TableSpecificColumnNameP table) (pchar ',' .>> spaces)
            )
        |>> (fun col -> table, col)
    )
    //.>. braceP columnListP
    .> pstringInsensitive "values"
    .>. braceP valueListP
    .> pchar ';'
    |> bindP (fun ((table,columns),data) ->
        let columns = List.map (fun c -> c.Col) columns
        match InsertStatement.create dbSchema table columns data with
        | Ok insertStatement -> returnP NoLabelSpecified insertStatement
        | Error errors ->
            failP (errors |> dbSpecificASTError |> CustomError)
            <?> NoLabelSpecified
        //match columns, data with
        //| (columns,data) when columns.Length = data.Length ->
        //    let statement = InsertStatement.create dbSchema table
        //    returnP NoLabelSpecified ()
        //| _ ->
        //    failP (InsertColumnAndDataCountMismatch(columns,data) |> SQLASTError |> CustomError)
        //    <?> NoLabelSpecified

    )
    //|> bindP (fun ((table,columns),data) ->
    //    let tablesOfColumns =
    //        columns
    //        |> List.map (fun c -> databaseParser.DatabaseSchema.GetColumnTable c.Col)
    //        |> List.distinct
    //    match tablesOfColumns, columns, data with
    //    // the only OK case...
    //    | [singleTable], cols, data when cols.Length = data.Length && singleTable = table ->
    //        returnP NoLabelSpecified ()
    //    // all the ways this can go wrong
    //    | nonSingleTable, _, _ ->
    //        fail ""
    //
    //
    //
    //    )
