module Tests

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


[<Tests>]
let tests =
    testList "SQLAST" [

        QueryTests.tests

        InsertTests.tests

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