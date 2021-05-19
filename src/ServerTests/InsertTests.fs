module InsertTests

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
open ProjectSpecificLabels

let tests =
    let projectAppDBP = databaseP DatabaseSchema.projectAppDBSchema
    let projectAppInsertP =
      insertP ProjectSpecificError.SQLASTError projectAppDBP

    let createInsertValue table columns =
        DatabaseAccess.createInsert table columns
        |> function
        | Ok insert -> insert
        | Error errs -> sprintf "Incorrect expected insert statement initialization! %A" errs |> failwith

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

    let insertParseTester str =
      run projectAppInsertP str
      |> stripParseResultPos

    let insertTest inputStr query =
        let forwardsFunc = insertParseTester
        let backwardsFunc = stringizeSQLInsert projectAppDBSchema >> Result.map (reParametrizeInsertString projectAppDBSchema)
        let label = sprintf "should parse query string: \"%s\"." inputStr
        testList "query" [
          testCase label <| fun _ ->
            let actual = insertParseTester inputStr
            let expected = Ok query
            Expect.equal actual expected ""
          roundtripTest forwardsFunc backwardsFunc inputStr
          triviaTests forwardsFunc inputStr
        ]


    testList "Insert" [
        // Note: NOT actually usefull or correct insert statements for the actual DB schema!
        // This only tests that a 'possible' insert statement should be recognized by the parser.
        insertTest
            "insert into User (User.UserNameID,User.UserName,User.UserID) values (\"userNameIDValue\",\"userNameValue\",11);"
            (
                createInsertValue UserTable [
                    UserCol UserNameID, String "userNameIDValue"
                    UserCol UserName, String "userNameValue"
                    UserCol UserID, Int 11
                ]
            )

        insertTest
            "insert into User (UserNameID,UserName,UserID) values (\"userNameIDValue\",\"userNameValue\",11);"
            (
                createInsertValue UserTable [
                    UserCol UserNameID, String "userNameIDValue"
                    UserCol UserName, String "userNameValue"
                    UserCol UserID, Int 11
                ]
            )

        insertTest
            "insert into OrganizationMember (User_ID) values (11);"
            (
                createInsertValue OrganizationMemberTable [
                    OrganizationMemberCol OrganizationMemberCol.User_ID, Int 11
                ]
            )
        insertTest
            "insert into LocalAuthentication (User_ID) values (11);"
            (
                createInsertValue LocalAuthenticationTable [
                    LocalAuthenticationCol LocalAuthenticationCol.User_ID, Int 11
                ]
            )
        insertTest
            "insert into LocalAuthentication (LocalAuthentication.User_ID) values (11);"
            (
                createInsertValue LocalAuthenticationTable [
                    LocalAuthenticationCol LocalAuthenticationCol.User_ID, Int 11
                ]
            )
    ]
