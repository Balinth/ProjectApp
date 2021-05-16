module QueryTests

open Expecto

open ResultExtensions
open ParserCombinator
open SQLAST
open DatabaseSchema
open SQLParser
open ExpressionEvaulating
open SQLGenerator
open WhiteSpacePermutations
open TestingUtils

let tests =
  let projectAppDBP = databaseP DatabaseSchema.projectAppDBSchema
  let projectAppQueryP = queryP projectAppDBP
  let projectAppBoolExprP = (expressionParsers projectAppDBP.ColumnNameP)

  let projectAppColumns = getDatabaseColumnCases<ProjectAppCol>()
  let userColumns =
    projectAppColumns
    |> Seq.filter (function | UserCol _ -> true | _ -> false )
    |> Seq.map (fun c -> {Col=c;Type=getColumnType c})
    |> List.ofSeq
  let projectColumns =
    projectAppColumns
    |> Seq.filter (function | ProjectTableCol _ -> true | _ -> false )
    |> Seq.map (fun c -> {Col=c;Type=getColumnType c})
    |> List.ofSeq

  let exprParserTester str =
        run (projectAppBoolExprP.FieldExprP |>> binaryFieldExprSolver) str
        |> function | Ok(Ok(Int a),b) -> Ok a | somethingElse -> Error somethingElse

  let exprParserTesterRational str =
        run (projectAppBoolExprP.FieldExprP |>> binaryFieldExprSolver) str
        |> function | Ok(Ok(Float a),b) -> Ok a | somethingElse -> Error somethingElse

  let boolExprParserTester str =
    run projectAppBoolExprP.BoolExprP str
    |> stripParseResultPos

  let boolExprParserEvalTester str =
    result {
      let! parsed,pos = run projectAppBoolExprP.BoolExprP str |> Result.mapError (fun e -> e :> obj)
      let! evaulationResult = boolExprSolver parsed |> Result.mapError (fun e -> e :> obj)
      return evaulationResult
    }

  let queryParseTester str =
    run projectAppQueryP str
    |> stripParseResultPos

  let exprTestCase inputString expectedResult =
    let testFunc = boolExprParserEvalTester
    let forwardsFunc = boolExprParserTester
    let backwardsFunc = stringizeExpression projectAppDBSchema (ctxFactory()) [] >> reParametrizeQueryString >> Ok
    let testFuncName = funName <@testFunc@>
    let caseLabel =
      sprintf "should parse \"%s\" with %s to the expected result." inputString testFuncName
    testList "expression" [
      testCase caseLabel <| fun _ ->
        let expected = Ok expectedResult
        let actual = testFunc inputString
        Expect.equal actual expected ""
      roundtripTest forwardsFunc backwardsFunc inputString
      triviaTests forwardsFunc inputString
    ]


  let queryTest inputStr query =
    let forwardsFunc = queryParseTester
    let backwardsFunc = stringizeSQLQuery projectAppDBSchema >> Result.map reParametrizeQueryString
    let label = sprintf "should parse query string: \"%s\"." inputStr
    testList "query" [
      testCase label <| fun _ ->
        let actual = queryParseTester inputStr
        let expected = Ok query
        Expect.equal actual expected ""
      roundtripTest forwardsFunc backwardsFunc inputStr
      triviaTests forwardsFunc inputStr
    ]


  testList "Query" [
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

    queryTest "select * from User" {Columns=userColumns;Condition=None}
    queryTest "select * from User, Project" {Columns=projectColumns @ userColumns;Condition=None}
    queryTest "select UserName, UserID from User" {Columns=[UserName|> UserCol |> getColumn; UserID |> UserCol |> getColumn];Condition=None}
    queryTest "select UserName, UserID from User where true" {Columns=[UserName|> UserCol |> getColumn; UserID |> UserCol |> getColumn];Condition=true|>BoolLiteral|>Some }
    queryTest
      "select UserName, UserID from User where UserID = 1"
        {
          Columns=[UserName|> UserCol |> getColumn; UserID |> UserCol |> getColumn]
          Condition=
            RelationExpr(
              UserID |> UserCol |> getColumn |> Column,
              Equals,
              Value <| Int 1
            ) |> Some
        }
    queryTest
      "select UserName, UserID from User where (UserID = 1 + 2 or UserID = 2) and UserName <> \"\" "
        {
          Columns=[UserName|> UserCol |> getColumn; UserID |> UserCol |> getColumn]
          Condition=
            BinaryBoolExpr(
              BracedBoolExpr(
                BinaryBoolExpr(
                  RelationExpr(
                    UserID |> UserCol |> getColumn |> Column,
                    Equals,
                    BinaryFieldExpr(
                      Value <| Int 1,
                      Add,
                      Value <| Int 2
                    )
                  ),
                  Or,
                  RelationExpr(
                    UserID |> UserCol |> getColumn |> Column,
                    Equals,
                    Value <| Int 2
                  )
                )
              ),
              And,
              RelationExpr(
                UserName |> UserCol |> getColumn |> Column,
                NotEquals,
                Value <| String ""
              )
            )
            |> Some
        }
  ]
