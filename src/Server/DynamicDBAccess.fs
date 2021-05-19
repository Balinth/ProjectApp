module DynamicDBAccess

open ParserCombinator
open ResultExtensions
open Shared
open SQLAST
open SQLParser
open SQLGenerator
open DatabaseSchema
open DatabaseAccess
open System.Collections.Generic
open DynamicTable
open ProjectSpecificLabels


let projectAppDBP = databaseP projectAppDBSchema
let projectAppQueryP = queryP projectAppDBP
let projectAppInsertP = insertP ProjectSpecificError.SQLASTError projectAppDBP

let tryGetRowValue key (dapperRow: IDictionary<string,obj> ) : Data =
    let keyFound, value = dapperRow.TryGetValue key
    if keyFound then
        match value with
        | :? float as float -> Float float
        | :? single as single -> float single |> Float
        | :? int as int ->  Int int
        | :? string as str -> String str
        | unexpectedType -> string unexpectedType |> String
    else
        String "-"

let dapperResultToDynamicRow (dbSchema: DatabaseSchema<'c,'t>) expectedColumns (dapperRow: IDictionary<string,obj> ) : Row =
    expectedColumns
    |> Seq.map (fun c -> tryGetRowValue (dbSchema.GetColumnName c) dapperRow)
    |> List.ofSeq
    |> (fun data -> {Data = data})

let dapperResultToDynamicTable (dbSchema: DatabaseSchema<'c,'t>) expectedColumns (dapperResult: IDictionary<string,obj> seq) : DynamicTable<'c> =
    dapperResult
    |> Seq.map (dapperResultToDynamicRow dbSchema expectedColumns)
    |> List.ofSeq
    |> (DynamicTable.create expectedColumns)
    |> Option.get // this should never fail here...

let disallowedColumns =
    [
        LocalAuthenticationCol LocalAuthID
        LocalAuthenticationCol PasswordHash
        LocalAuthenticationCol Salt
        LocalAuthenticationCol User_ID
    ]
    |> List.map projectAppDBSchema.GetColumn

let transformQuery (query:QueryStatement<ProjectAppCol>) =
    {
        Columns =
            query.Columns
            |> List.filter (fun c -> not <| List.contains c disallowedColumns)
        Condition = query.Condition
    }

let query sqlString (userAuth : Security.UserAuthInfo) : QueryResult =

    result {
        let! query, input =
            run projectAppQueryP sqlString
            |> Result.mapError (fun e -> [ParserError e])
            |> Result.map (fun (q,i) -> transformQuery q, i)
        let! sqlString =
            stringizeSQLQuery projectAppDBSchema query
            |> Result.mapError (List.map (SQLError >> DBError))
        let! queryResult =
            executeQuery sqlString
            |> Result.map (fun rows -> Seq.cast rows)
            |> Result.mapError (List.map (DBError))
        let table = dapperResultToDynamicTable projectAppDBSchema (query.Columns |> List.map (fun c -> c.Col)) queryResult
        return table
    }

    //let result =
    //    run projectAppQueryP sqlString
    //    >>= (fst >> stringizeSQLQuery projectAppDBSchema)
    //    |> Result.mapError (fun eList -> List.map SQLError eList)
    //    >>= executeQuery
    //    |>> (dapperResultToDynamicTable projectAppDBSchema)


    //()

let insert insertSQL (userAuth : Security.UserAuthInfo) : Result<unit,APIError list> =
    result {
        let! insert, input =
            run projectAppInsertP insertSQL
            |> Result.mapError (fun e -> [ParserError e])
        let! sqlString =
            SQLGenerator.stringizeSQLInsert projectAppDBSchema insert
            |> Result.mapError (List.map (SQLError >> DBError))

        let! inserResult =
            executeInsert projectAppDBSchema sqlString
            |> Result.mapError (List.map (DBError))
            |> Result.map ignore

        return inserResult
    }