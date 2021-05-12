module WhiteSpacePermutations

open Expecto

open ParserCombinator

let isLetterOrDigitOrQuote c =
    System.Char.IsLetterOrDigit c
    || c = '\"'

let segmentP =
    let wordP =
        attemptP pfloat |>> ignore
        <|> (attemptP pint |>> ignore)
        <|> (manyChars1 (satisfy (fun c -> isLetterOrDigitOrQuote c || c = '_') (BasicLabel.String "word") ) |>> ignore)
    getPosP
    .>> (sepBy1 wordP spaces |>> ignore)
    .>>. getPosP

let specialCharP =
    let anyOtherSpecialP =
        fun c ->
            System.Char.IsLetterOrDigit c = false
            && System.Char.IsWhiteSpace c = false
        |> satisfy <| NoLabelSpecified
        |>> string
    let specialCharPs =
        [
            "+="
            "++"
            "+"
            "-="
            "--"
            "-"
            "*="
            "*"
            "/="
            "/"
            "%="
            "%"
            "\\"
            "?"
            ":"
            "."
            ","
            ";"
            "|"
            "="
            "<>"
            "<="
            ">="
            "<"
            ">"
            "("
            ")"
        ]
        |> List.map (pstring >> attemptP)
        |> List.append <| [anyOtherSpecialP]
        |> choice

    spaces
    >>. getPosP
    .>> specialCharPs
    .>>. getPosP
    .>> spaces

let getSpecialChars =
    segmentP
    <|> specialCharP
    |> many1
    //|>> List.map (fun (a, b) -> [a;b] )
    //|>> List.concat
    .>>. (opt segmentP)
    |>> (fun (list,tail) ->
        match tail with
        | Some tail -> List.append list [tail]
        | None -> list)

open System.Text

let rec buildStr ((sb:StringBuilder),(current:Input)) (target:Input) =
    match TextInput.nextChar current, target with
    // we finished with folding to this target input
    | (newCurrent, Some char), target when newCurrent = target ->
         sb.Append(char), newCurrent
    // there was a character to consume, and we are still not at the target
    | (newCurrent, Some char), target (*when newCurrent <> target*) ->
        buildStr (sb.Append(char),newCurrent) target
    // there was no more input to be consumed, we "finished", jump to target
    | (_, None), _ -> sb,target

let foldInputPairs (sb:StringBuilder) inputPair =
    buildStr (sb,fst inputPair) (snd inputPair)
    |> fst


let foldInputPairsPad (sb:StringBuilder) inputPair =
    buildStr (sb.Append(" "),fst inputPair) (snd inputPair)
    |> fst

let foldInputPairsHalfPad (sb:StringBuilder,pad) inputPair =
    buildStr (sb.Append(if pad then " " else "" ),fst inputPair) (snd inputPair)
    |> fst , not pad

let createNakedString (fromTos:(Input*Input) list) =
    fromTos
    |> List.fold foldInputPairs (StringBuilder())
    |> string

let createLeftPaddedString (fromTos:(Input*Input) list) =
    fromTos
    |> List.fold foldInputPairsHalfPad (StringBuilder(),false)
    |> fst
    |> (fun (sb:StringBuilder) -> sb.ToString())

let createRightPaddedString (fromTos:(Input*Input) list) =
    fromTos
    |> List.fold foldInputPairsHalfPad (StringBuilder(),true)
    |> fst
    |> (fun (sb:StringBuilder) -> sb.ToString())

let createPaddedString (fromTos:(Input*Input) list) =
    fromTos
    |> List.fold foldInputPairsPad (StringBuilder())
    |> string

let withWhitespacePermutations (inputStr:string) =
  [
    run (getSpecialChars |>> createNakedString)
    run (getSpecialChars |>> createPaddedString)
    run (getSpecialChars |>> createLeftPaddedString)
    run (getSpecialChars |>> createRightPaddedString)
  ]
  |> List.map (fun generator ->
      generator inputStr
      |> Expect.wantOk <| "fail"
      |> fst
      |> fun s -> s.TrimStart() // most parsers expect the string starts without whitespace
      )
