module Tests

open Microsoft.FSharp.Quotations

open Expecto

open ParserCombinator
open SQLAST
open SQLParser
open SQLGenerator
open ResultExtensions


let rec funName = function
| Patterns.Call(None, methodInfo, _) -> methodInfo.Name
| Patterns.Lambda(_, expr) -> funName expr
| Patterns.ValueWithName(expr,valType,name) -> expr.GetType().ToString()
| somethingElse -> sprintf "Unexpected input: %A" somethingElse |> failwith

[<Tests>]
let tests =
  let exprParserTester str =
        run (projectAppBoolExprP.FieldExprP |>> binaryFieldExprSolver) str
        |> function | Ok(Ok(Int a),b) -> Ok a | somethingElse -> Error somethingElse

  let exprParserTesterRational str =
        run (projectAppBoolExprP.FieldExprP |>> binaryFieldExprSolver) str
        |> function | Ok(Ok(Float a),b) -> Ok a | somethingElse -> Error somethingElse

  let boolExprParserTester str =
    run projectAppBoolExprP.BoolExprP str
    |> function | Ok(a,b) -> Ok a | somethingElse -> Error somethingElse
  
  let boolExprParserEvalTester str =
    result {
      let! parsed,pos = run projectAppBoolExprP.BoolExprP str |> Result.mapError (fun e -> e :> obj)
      let! evaulationResult = boolExprSolver parsed |> Result.mapError (fun e -> e :> obj)
      return evaulationResult
    }

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

  let stringizeData data =
    match data with
    | Int i -> string i
    | Data.Float f -> if f % 1.0 = 0.0 then string f + ".0" else string f
    | Data.String s -> sprintf "\"%s\"" s
  
  let reParametrizeSQLString (str, parameters) =
    List.fold (fun (str:string) parameter ->
      str.Replace(stringizeParameterName parameter, stringizeData parameter.ParamValue)
      ) str parameters

  let exprTestCase inputString expectedResult =
    let testFunc = boolExprParserEvalTester
    let forwardsFunc = boolExprParserTester
    let backwardsFunc = stringizeExpression projectAppDB (ctxFactory()) [] >> reParametrizeSQLString >> Ok
    let testFuncName = funName <@testFunc@>
    let caseLabel =
      sprintf "should parse \"%s\" with %s to the expected result." inputString testFuncName
    testList "expression" [
      testCase caseLabel <| fun _ ->
        let expected = Ok expectedResult
        let actual = testFunc inputString
        Expect.equal actual expected ""
      roundtripTest forwardsFunc backwardsFunc inputString
    ]

  testList "SQLParser" [
    testCase "should parse single number" <| fun _ ->
      let expected = Ok 1
      let actual = exprParserTester "1"
      Expect.equal actual expected "should parse \"1\" as the Integer literal 1"
    testCase "should parse single rational number" <| fun _ ->
      let expected = Ok 1.5
      let actual = exprParserTesterRational "1.5"
      Expect.equal actual expected "should parse \"1.5\" as the foating point literal 1.5"
    testCase "should parse and solve addition" <| fun _ ->
      let expected = Ok 2
      let actual = exprParserTester "1+1"
      Expect.equal actual expected "should parse \"1+1\" as the Integer literal 2"
    testCase "should parse and solve addition of rationals" <| fun _ ->
      let expected = Ok 2.0
      let actual = exprParserTesterRational "1.5+0.5"
      Expect.equal actual expected "should parse \"1.5+0.5\" as the Integer literal 2"
    testCase "should parse and solve multiplication" <| fun _ ->
      let expected = Ok 6
      let actual = exprParserTester "3*2"
      Expect.equal actual expected "should parse \"3*2\" as the Integer literal 6"
    testCase "should parse and solve division" <| fun _ ->
      let expected = Ok 2
      let actual = exprParserTester "6/3"
      Expect.equal actual expected "should parse \"6/3\" as the Integer literal 2"
    testCase "should parse and solve multiplication with greater precedence than addition" <| fun _ ->
      let expected = Ok 2.0
      let actual = exprParserTesterRational "1+2*0.5"
      Expect.equal actual expected "should parse \"1+2*0.5\" as the Integer literal 2"
    testCase "should parse and solve division with greater precedence than addition" <| fun _ ->
      let expected = Ok 3
      let actual = exprParserTester "1+6/3"
      Expect.equal actual expected "should parse \"1+6/3\" as the Integer literal 3"
    testCase "should parse braced expressions first" <| fun _ ->
      let expected = Ok 2.0
      let actual = exprParserTesterRational "(1+2*0.5)*2-2"
      Expect.equal actual expected "should parse \"(1+2*0.5)*2-2\" as the literal 2"
    
    testCase "should parse true and false" <| fun _ ->
      let expected = BinaryBoolExpr(BoolLiteral true,And,BoolLiteral false) |> Ok
      let actual = boolExprParserTester "true and false"
      Expect.equal actual expected ""
    testCase "should parse multiple ands" <| fun _ ->
      let expected = BinaryBoolExpr(BinaryBoolExpr(BoolLiteral true,And,BoolLiteral true), And, BoolLiteral false) |> Ok
      let actual = boolExprParserTester "true and true and false"
      Expect.equal actual expected ""
    testCase "should parse and with higher precedence than or" <| fun _ ->
      let expected = BinaryBoolExpr(BoolLiteral true, Or, BinaryBoolExpr(BoolLiteral true,And,BoolLiteral false)) |> Ok
      let actual = boolExprParserTester "true or true and false"
      Expect.equal actual expected ""
      
    testCase "should parse true and false, eval it to false" <| fun _ ->
      let expected = Ok false
      let actual = boolExprParserEvalTester "true and false"
      Expect.equal actual expected ""
    testCase "should parse true or false, eval it to true" <| fun _ ->
      let expected = Ok true
      let actual = boolExprParserEvalTester "true or false"
      Expect.equal actual expected ""
    testCase "should parse multiple ands, and eval to false" <| fun _ ->
      let expected = Ok false
      let actual = boolExprParserEvalTester "true and true and false"
      Expect.equal actual expected ""
    testCase "should parse and with higher precedence than or and eval to true" <| fun _ ->
      let expected = Ok true
      let actual = boolExprParserEvalTester "true or true and false"
      Expect.equal actual expected ""
    testCase "should parse braced or with higher precedence than and, and eval to false" <| fun _ ->
      let expected = Ok false
      let actual = boolExprParserEvalTester "(true or true) and false"
      Expect.equal actual expected ""
      
    exprTestCase "0 < 1" true
    exprTestCase "0 <= 0" true
    exprTestCase "0 < -1" false
    exprTestCase "0 > 1" false
    exprTestCase "1.0 = 1" true
    exprTestCase "1.1 = 1" false
    exprTestCase "0.9 = 1" false
    exprTestCase "1 = 1.0" true
    exprTestCase "1.1 = 1.1" true
    exprTestCase "0 <> 1" true
    exprTestCase "0.5 <> 0.5" false
    exprTestCase "false and 1=1" false
    exprTestCase "1=1 and false" false
    exprTestCase "1+1*2=3 and 3-1+2=4" true
    exprTestCase "1+1.5*2=4.0 and 3-(-1+2.5*2)=-1.0" true
    exprTestCase "\"1\"=1" true
    exprTestCase "1=\"1\"" true
    exprTestCase "\"1\"=\"1\"" true
    exprTestCase "\"fisfos\"=\"fisfos\"" true
    exprTestCase "\"a\"<\"b\"" true
    exprTestCase "\"1\"=1 and 1.0 = 1 and 1 = 1.0 and " true
    //exprTestCase boolExprParserTester "1=1 and false" (BinaryBoolExpr(RelationExpr(Int 1 |> Value,Equals,Int 1 |> Value),And,BoolLiteral false))

    // sample tests for reference
    // the ptestCase function ignores them
    ptestCase "universe exists (╭ರᴥ•́)" <| fun _ ->
      let subject = true
      Expect.isTrue subject "I compute, therefore I am."

    ptestCase "when true is not (should fail)" <| fun _ ->
      let subject = false
      Expect.isTrue subject "I should fail because the subject is false"

    ptestCase "I'm skipped (should skip)" <| fun _ ->
      Tests.skiptest "Yup, waiting for a sunny day..."

    ptestCase "I'm always fail (should fail)" <| fun _ ->
      Tests.failtest "This was expected..."

    ptestCase "contains things" <| fun _ ->
      Expect.containsAll [| 2; 3; 4 |] [| 2; 4 |]
                         "This is the case; {2,3,4} contains {2,4}"

    ptestCase "contains things (should fail)" <| fun _ ->
      Expect.containsAll [| 2; 3; 4 |] [| 2; 4; 1 |]
                         "Expecting we have one (1) in there"

    ptestCase "Sometimes I want to ༼ノಠل͟ಠ༽ノ ︵ ┻━┻" <| fun _ ->
      Expect.equal "abcdëf" "abcdef" "These should equal"

    ptest "I am (should fail)" {
      "╰〳 ಠ 益 ಠೃ 〵╯" |> Expect.equal true false
    }
  ]
