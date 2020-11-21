module Tests

open Expecto

open ParserCombinator
open SQLAST
open SQLParser
(*

run (projectAppColumnP) "UserName"
run (projectAppBoolExprP.FieldExprP |>> binaryFieldExprSolver) "1"
run (projectAppBoolExprP.FieldExprP |>> binaryFieldExprSolver) "1+1-1"
run (projectAppBoolExprP.FieldExprP |>> binaryFieldExprSolver) "-1+1-1"
run (projectAppBoolExprP.FieldExprP |>> binaryFieldExprSolver) "1-2-3)"
run (projectAppBoolExprP.FieldExprP |>> binaryFieldExprSolver) "1-(2-3)"
run (projectAppBoolExprP.FieldExprP |>> binaryFieldExprSolver) "1+2*3"
run (projectAppBoolExprP.FieldExprP |>> binaryFieldExprSolver) "1*2+3"
run (projectAppBoolExprP.FieldExprP |>> binaryFieldExprSolver) "(1+2)*3"
run (projectAppBoolExprP.BoolExprP) "false and not true"

*)

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
