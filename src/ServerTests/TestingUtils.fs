module TestingUtils

open Microsoft.FSharp.Quotations

open Expecto

open ResultExtensions
open ParserCombinator
open SQLAST
open DatabaseSchema
open SQLParser
open ExpressionEvaulating
open SQLGenerator
open WhiteSpacePermutations

// utility functions
let rec funName = function
| Patterns.Call(None, methodInfo, _) -> methodInfo.Name
| Patterns.Lambda(_, expr) -> funName expr
| Patterns.ValueWithName(expr,valType,name) -> expr.GetType().ToString()
| somethingElse -> sprintf "Unexpected input: %A" somethingElse |> failwith

let stripParseResultPos = function | Ok(a,b) -> Ok(a) | Error err -> Error err

let stringizeData data =
  match data with
  | Int i -> string i
  | Data.Float f -> if f % 1.0 = 0.0 then string f + ".0" else string f
  | Data.String s -> sprintf "\"%s\"" s

let reParametrizeQueryString (str, parameters) =
  List.fold (fun (str:string) parameter ->
    str.Replace(stringizeParameterName parameter, stringizeData parameter.ParamValue)
    ) str parameters

let reParametrizeInsertString db (str, parameters) =
  List.fold (fun (str:string) parameter ->
    str.Replace(stringizeInsertParameterName db parameter, stringizeData parameter.Value)
    ) str parameters

let roundtripTest forwardFun backwardFun input =
    let forwardName = funName <@forwardFun@>
    let backwardName = funName <@backwardFun@>
    let listLabel = sprintf "Roundtrip between %s and %s on input: %s" forwardName backwardName input
    let forwardLabel = sprintf "Forward trip with %s on input: %s" forwardName input
    let backwardLabel = sprintf "Backward trip with %s on input: %s" backwardName input
    testSequenced <| testList listLabel [
      testCase forwardLabel <| fun _ ->
        Expect.isOk (forwardFun input) ""
      testCase backwardLabel <| fun _ ->
        let (forwardResult:'a) = forwardFun input |> Expect.wantOk <| "Failed within first forwardFun"
        let backwardResult = backwardFun forwardResult |> Expect.wantOk <| "Failed within backwardFun"
        let roundTripResult =
          forwardFun backwardResult |> Expect.wantOk
          <| "Failed within second forwardFun, regenerated input was: " + backwardResult
        Expect.equal roundTripResult forwardResult "Roundtrip result not equal to first pass."
    ]

let triviaTests testFunc inputStr =
    let originalResult = testFunc inputStr
    let permutations = withWhitespacePermutations inputStr
    permutations
    |> List.map (fun inputPermutation ->
      let label = sprintf "should parse permutation \"%s\" to the same tree as original \"%s\"" inputPermutation inputStr
      testCase label (fun _ ->
        let expected = originalResult |> Expect.wantOk <| "failed at creating the baseline result"
        let actual = testFunc inputPermutation |> Expect.wantOk <| "failed at creating the permutation based result"
        Expect.equal actual expected ""
        )
      )
    |> testList "trivia permutations"
