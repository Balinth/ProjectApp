module ParserCombinator
(*
Adapted from Sctott Wlaschin's excellent blog series:
Understanding parser combinators: https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/
Main modifications: 
    *Typed base parser errors (eg.: no more input, etc), also delegates parser labeling. (eg no auto generated string labels)
    *nextChar returns None instead of '\n' at end of file, if there was no closing new line. Note: this is not strictrly a bugfix, as Scott Wlaschin saw this as a feature in the original implementation. 
    *many combinator is optimised to use constant stack count
*)

// Label parametrization is done through ProjectSpecificLabel type.
// In a different project, a different .fs file containing a different ProjectSpecificLabel can be included.
open ProjectSpecificLabels
type BasicLabel =
    | NoLabelSpecified
    | Recursive
    | Attempt of BasicLabel
    | AndThen of BasicLabel * BasicLabel
    | OrElse of BasicLabel * BasicLabel
    | Many of BasicLabel
    | Optional of BasicLabel
    | String of string
    | CaseInsensitiveString of string
    | Char of char
    | Integer
    | DigitChar
    | Float
    | EmptySequence
    | WhiteSpace
    | Inside of parent: BasicLabel * local: BasicLabel
    | CustomLabel of ProjectSpecificLabel

type BasicParserError =
    | NoMoreInput
    | UnexpectedChar of char
    | Int32Overflow of string
    | Float64Overflow of string
    | NonFatal of BasicParserError
    //| CustomError of ProjectSpecificError

module TextInput =
    open System

    type Position = {
        Line : int
        Column : int
        CharIndex: int
    }
    
    let initialPos = {Line=0; Column=0; CharIndex=0}

    let incrCol pos = 
        {pos with Column=pos.Column + 1; CharIndex=pos.CharIndex+1}

    let incrLine pos = 
        {Line=pos.Line + 1; Column=0; CharIndex = pos.CharIndex+1}

    type InputState = {
        Lines : string[]
        Position : Position 
    }

    let currentLine inputState = 
        let linePos = inputState.Position.Line
        if linePos < inputState.Lines.Length then
            inputState.Lines.[linePos]
        else
            "end of file"

    let fromStr str = 
        if String.IsNullOrEmpty(str) then
            {Lines=[||]; Position=initialPos}
        else
            let separators = [| "\r\n"; "\n" |]
            let lines = str.Split(separators, StringSplitOptions.None)
            {Lines=lines; Position=initialPos}


    /// Get the next character from the input, or None if reached the end of input.
    /// also gives back a new updated input state.
    let nextChar input =
        let linePos = input.Position.Line
        let colPos = input.Position.Column

        if linePos >= input.Lines.Length then
            input, None
        else
            let currentLine = currentLine input
            if colPos < currentLine.Length then
                let char = currentLine.[colPos]
                let newPos = incrCol input.Position 
                let newState = {input with Position=newPos}
                newState, Some char
            else
                if linePos = input.Lines.Length - 1 then
                    // end of the last line, and so end of the input.
                    input, None
                else
                    // end of intermediate line, so return LF and move to next line
                    let char = '\n'
                    let newPos = incrLine input.Position 
                    let newState = {input with Position=newPos}
                    newState, Some char

// ===========================================
// Parser code
// ===========================================

open System
open System.Globalization

// Aliases for input, etc
type Input = TextInput.InputState   // type alias

/// Stores information about the parser position for error messages
type ParserPosition = {
    CurrentLine : string
    Line : int
    Column : int
    CharIndex : int
    }

type ParserResult<'a,'parserLabel,'parserError> = FSharp.Core.Result<'a * Input,'parserLabel*'parserError*ParserPosition>

/// A Parser structure has a parsing function & label
type Parser<'a,'parserLabel,'parserError> = {
    ParseFn : (Input -> ParserResult<'a,'parserLabel,'parserError>)
    Label:  'parserLabel 
    }

/// Run the parser on a InputState
let runOnInput parser input = 
    parser.ParseFn input

/// Run the parser on a string
let run parser inputStr = 
    TextInput.fromStr inputStr
    |> runOnInput parser

// =============================================
// Error messages
// =============================================

let parserPositionFromInputState (inputState:Input) = {
    CurrentLine = TextInput.currentLine inputState
    Line = inputState.Position.Line
    Column = inputState.Position.Column
    CharIndex = inputState.Position.CharIndex
    }

let printResult labelPrinter errorPrinter result =
    match result with
    | Ok (value,input) -> 
        printfn "%A" value
    | Error (label,error,parserPos) ->
        let label = labelPrinter label
        let error = errorPrinter error
        let errorLine = parserPos.CurrentLine
        let colPos = parserPos.Column
        let linePos = parserPos.Line
        let failureCaret = sprintf "%*s^%s" colPos "" error
        // examples of formatting
        //   sprintf "%*s^%s" 0 "" "test"
        //   sprintf "%*s^%s" 10 "" "test"
        printfn "Line:%i Col:%i Error parsing %s\n%s\n%s" linePos colPos label errorLine failureCaret 
  

// =============================================
// Label related
// =============================================

/// get the label from a parser
let getLabel parser = 
    // get label
    parser.Label

/// update the label in the parser
let setLabel parser newLabel = 
    // change the inner function to use the new label
    let newInnerFn input = 
        let result = parser.ParseFn input
        match result with
        | Ok s ->
            // if Success, do nothing
            Ok s 
        | Error (oldLabel,err,pos) -> 
            // if Failure, return new label
            Error (newLabel,err,pos) 
    // return the Parser
    {ParseFn=newInnerFn; Label=newLabel}

/// infix version of setLabel
let ( <?> ) = setLabel
let ( <??> ) parser label = setLabel parser (Inside (label, getLabel parser))

// =============================================
// Standard combinators
// =============================================

/// Match an input token if the predicate is satisfied
let satisfy predicate label =
    let innerFn input =
        let remainingInput,charOpt = TextInput.nextChar input 
        match charOpt with
        | None -> 
            let err = NoMoreInput
            let pos = parserPositionFromInputState input
            Error (label,err,pos)
        | Some first -> 
            if predicate first then
                Ok (first,remainingInput)
            else
                let err = UnexpectedChar first
                let pos = parserPositionFromInputState input
                Error (label,err,pos)
    // return the parser
    {ParseFn=innerFn;Label=label}

let attemptP p =
    let innerFn input =
        match runOnInput p input with
        | Ok result -> Ok result
        | Error (label,err,pos) -> Error(label,NonFatal err, pos)
    {ParseFn=innerFn;Label=getLabel p |> Attempt}


/// "bindP" takes a parser-producing function f, and a parser p
/// and passes the output of p into f, to create a new parser
let bindP f p =
    let innerFn input =
        let result1 = runOnInput p input 
        match result1 with
        | Error (label,err,pos) -> 
            // return error from parser1
            Error (label,err,pos)  
        | Ok (value1,remainingInput) ->
            // apply f to get a new parser
            let p2 = f value1
            // run parser with remaining input
            runOnInput p2 remainingInput
    {ParseFn=innerFn; Label=getLabel p}

/// Infix version of bindP
let ( >>= ) p f = bindP f p

/// Lift a value to a Parser
let returnP label x = 
    let innerFn input =
        // ignore the input and return x
        Ok (x,input)
    // return the inner function
    {ParseFn=innerFn; Label=label}

let failP errorMsg =
    let innerFn input =
        Error ((),errorMsg,parserPositionFromInputState input)
    {ParseFn=innerFn;Label=()}

/// apply a function to the value inside a parser
let mapP f p = 
    bindP (f >> returnP (getLabel p)) p

/// infix version of mapP
let ( <!> ) = mapP

/// "piping" version of mapP
let ( |>> ) x f = mapP f x

/// apply a wrapped function to a wrapped value
let applyP fP xP =         
    (fP) >>= (fun f -> 
    (xP) >>= (fun x -> 
        returnP (getLabel fP) (f x) ))

/// infix version of apply
let ( <*> ) a b = applyP a b

/// lift a two parameter function to Parser World
let lift2 labelFun f xP yP =
    returnP (labelFun (getLabel xP) (getLabel yP)) f <*> xP <*> yP

/// Combine two parsers as "A andThen B"
let andThen p1 p2 =         
    let label = AndThen ((getLabel p1), (getLabel p2))
    p1 >>= (fun p1Result -> 
    p2 >>= (fun p2Result -> 
        returnP label (p1Result,p2Result) ))
    <?> label
    
/// Infix version of andThen
let ( .>>. ) = andThen

/// Combine two parsers as "A orElse B"
/// If the first parser fails with NonFatal error case after consuming input, the parser backtracks and tries the second one.
let orElse p1 p2 =
    let outerLabel = OrElse ((getLabel p1), (getLabel p2))
    let innerFn (input:Input) =
        let startingPos = input.Position
        // run parser1 with the input
        let result1 = runOnInput p1 input
        // test the result for Failure/Success
        match result1 with
        | Ok _ -> 
            // if success, return the original result
            result1
        | Error (label,err,pos) when startingPos.CharIndex = pos.CharIndex -> 
            // if failed without consuming input, run parser2 with the input
            let result2 = runOnInput p2 input
            // return parser2's result, overwriting label if it still failed
            match result2 with
            | Ok _ -> result2
            | Error (_,error,pos) when startingPos.CharIndex = pos.CharIndex -> Error(outerLabel,error,pos)
            | Error (label,error,pos) -> Error(Inside(outerLabel,label),error,pos)
        | Error (label,NonFatal err,pos) -> // backtrack and still try the second parser if the first failed with nonFatal error.
            // if failed without consuming input, run parser2 with the input
            let result2 = runOnInput p2 input
            // return parser2's result, overwriting label if it still failed
            match result2 with
            | Ok _ -> result2
            | Error (_,error,pos) when startingPos.CharIndex = pos.CharIndex -> Error(outerLabel,error,pos)
            | Error (label,error,pos) -> Error(Inside(outerLabel,label),error,pos)
        | Error (label,err,pos) -> // fail if the first parser failed after consuming input
            printfn "startPos: %A currPos: %A" startingPos pos
            Error(Inside (outerLabel,label),err,pos)
    // return the inner function
    {ParseFn=innerFn; Label=outerLabel}

/// Infix version of orElse
let ( <|> ) a b = orElse a b

/// Choose any of a list of parsers
let choice listOfParsers =
    List.reduce orElse listOfParsers


let rec sequence parserList =
    // define the "cons" function, which is a two parameter function
    let cons head tail = head::tail
    // lift it to Parser World
    let consP = lift2 (fun a b -> AndThen (a,b)) cons
    // process the list of parsers recursively
    match parserList with
    | [] -> 
        returnP EmptySequence []
    | head::tail ->
        consP head (sequence tail)

/// (helper) match zero or more occurences of the specified parser
let rec parseZeroOrMore values parser input =
    // run parser with the input
    match runOnInput parser input with
    | Error _ ->
        (values, input)
    | Ok (newValue, remainingInput) ->
        parseZeroOrMore (newValue::values) parser remainingInput

/// matches zero or more occurences of the specified parser
let many parser = 
    let rec innerFn input =
        // parse the input -- wrap in Success as it always succeeds
        let values, input = parseZeroOrMore [] parser input
        Ok (List.rev values, input)
    {ParseFn=innerFn; Label=Many (getLabel parser)}

/// matches one or more occurences of the specified parser
let many1 p =
    p >>= (fun head -> 
    many p >>= (fun tail -> 
        returnP (Many (getLabel p)) (head::tail) ))
    <?> (Many (getLabel p))

/// Parses an optional occurrence of p and returns an option value.
let opt p =
    let label = getLabel p |> Optional
    let some = p |>> Some
    let none = returnP label None
    some <|> none <?> label

/// Keep only the result of the left side parser
let (.>>) p1 p2 = 
    // create a pair
    p1 .>>. p2
    // then only keep the first value
    |> mapP (fun (a,b) -> a) 

/// Keep only the result of the right side parser
let (>>.) p1 p2 = 
    // create a pair
    p1 .>>. p2
    // then only keep the second value
    |> mapP (fun (a,b) -> b) 

/// Keep only the result of the middle parser
let between p1 p2 p3 =
    p1 >>. p2 .>> p3

/// Parses one or more occurrences of p separated by sep
let sepBy1 p sep =
    let sepThenP = sep >>. p           
    p .>>. (many sepThenP )
    |>> fun (p,pList) -> p::pList

/// Parses zero or more occurrences of p separated by sep
let sepBy p sep =
    (sepBy1 p sep) <|> (returnP (getLabel p |> Optional) [])

// =============================================
// Standard parsers 
// =============================================


// ------------------------------
// char and string parsing
// ------------------------------
            
/// parse a char 
let pchar charToMatch = 
    // label is just the character
    let label = BasicLabel.Char charToMatch 

    let predicate ch = (ch = charToMatch) 
    satisfy predicate label 

/// Choose any of a list of characters
let anyOf listOfChars = 
    listOfChars
    |> List.map pchar // convert into parsers
    |> choice

/// Convert a list of chars to a string
let charListToStr charList =
    System.String(List.toArray charList) 

/// Parses a sequence of zero or more chars with the char parser cp. 
/// It returns the parsed chars as a string.
let manyChars cp =
    many cp
    |>> charListToStr

let testStr =
    let sb = System.Text.StringBuilder()
    [0..50000]
    |> List.map (fun _ -> sb.Append("a"))
    |> ignore
    sb.ToString()

/// Parses a sequence of one or more chars with the char parser cp. 
/// It returns the parsed chars as a string.
let manyChars1 cp =
    many1 cp
    |>> charListToStr

/// parse a specific string
let pstring str = 
    // label is just the string
    let label = BasicLabel.String str 

    str
    // convert to list of char
    |> List.ofSeq
    // map each char to a pchar
    |> List.map pchar 
    // convert to Parser<char list>
    |> sequence
    // convert Parser<char list> to Parser<string>
    |> mapP charListToStr
    |> attemptP
    <?> label

let pstringInsensitive str =
    // label is just the string
    let label = CaseInsensitiveString str

    str
    // convert to list of char
    |> List.ofSeq
    // map each char to a pchar
    |> List.map (fun c -> pchar (System.Char.ToLower(c)) <|> pchar (System.Char.ToUpper(c))) 
    // convert to Parser<char list>
    |> sequence
    // convert Parser<char list> to Parser<string>
    |> mapP charListToStr
    <?> label


// ------------------------------
// whitespace parsing
// ------------------------------

/// parse a whitespace char
let whitespaceChar = 
    let predicate = Char.IsWhiteSpace 
    satisfy predicate WhiteSpace

/// parse zero or more whitespace char
let spaces : Parser<char list, BasicLabel, BasicParserError> = many whitespaceChar

/// parse one or more whitespace char
let spaces1 = many1 whitespaceChar



// ------------------------------
// number parsing
// ------------------------------

/// parse a digit
let digitChar = 
    let predicate = Char.IsDigit
    satisfy predicate DigitChar 


// parse an integer
let pint =
    // helper
    let resultToInt (sign,(digits:string)) =
        let isValid, i = digits |> System.Int32.TryParse
        match isValid, sign with
        | false, _ -> Int32Overflow digits |> failP <?> Integer
        | _, Some ch -> returnP Integer -i  // negate the int
        | _, None -> returnP Integer i
            
    // define parser for one or more digits
    let digits = manyChars1 digitChar

    // an "int" is optional sign + one or more digits
    opt (pchar '-') .>>. digits 
    >>= resultToInt
    <?> Integer

// parse a float
let pfloat =
    // helper
    let resultToFloat (((sign,digits1),point),digits2) =
        let floatStr = (sprintf "%s.%s" digits1 digits2)
        let isValid, fl = System.Double.TryParse(floatStr, Globalization.NumberStyles.Any, CultureInfo.InvariantCulture)
        match isValid, sign with
        | false, _ -> Float64Overflow floatStr |> failP <?> Float
        | _, Some ch ->  returnP Float -fl  // negate the float
        | _, None ->  returnP Float fl
            
    // define parser for one or more digits 
    let digits = manyChars1 (digitChar <|> whitespaceChar ) 
    // a float is sign, digits, point, digits (ignore exponents for now)
    let signP = opt (pchar '-')
    let decimalPointP = (pchar '.') <|> (pchar ',')
    signP .>>. digits .>>. decimalPointP .>>. digits 
    >>= resultToFloat <?> Float
    
// helper function for recursive parser definitions
let createParserForwardedToRef<'a,'parserError>() =

    let dummyParser= 
        let innerFn input : ParserResult<'a,'parserLabel,'parserError> = failwith "unfixed forwarded parser"
        {ParseFn=innerFn; Label=Recursive}
    
    // ref to placeholder Parser
    let parserRef = ref dummyParser 

    // wrapper Parser
    let innerFn input = 
        // forward input to the placeholder
        runOnInput !parserRef input 
    let wrapperParser = {ParseFn=innerFn; Label=Recursive}

    wrapperParser, parserRef

// usage example for recursive tree parsing
(*
let (<||>) a b = orElseL (fun l -> sprintf "Expected %s or else %s" (fst l) (snd l)) a b
type Tree =
    | Node of string * Tree * Tree
    | Leaf of string
let leafP = pstring "leaf" |>> Leaf <?> "leaf"
let nodeP, nodePRef = createParserForwardedToRef<Tree,BasicParserError>()
let treeP =
    nodeP <||> leafP
nodePRef := pstring "node" .>>. treeP .>>. treeP |>> (fun ((a,b),c) -> Node (a, b, c)) <?> "node"
run treeP "nodeleafnodeleafleaf"
*)

