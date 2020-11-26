module ExpressionEvaulating

open SQLAST
open ResultExtensions

type ExprError =
    | InvalidOperand of BinaryNumericOp * Data
    | Errors of ExprError list
    | DivisionByZero
    | CantSolveColumnRef
    | InvalidLiteralCast of Original: Data * TargetType: Data

let addData left right =
    match left with
    | Int l ->
        match right with
        | Int r -> l + r |> Int
        | Data.Float r -> float l + r |> Data.Float
        | Data.String r -> string l + r |> Data.String
    | Data.Float l ->
        match right with
        | Int r -> l + float r |> Data.Float
        | Data.Float r -> l + r |> Data.Float
        | Data.String r -> string l + r |> Data.String
    | Data.String l ->
        match right with
        | Int r -> l + string r |> Data.String
        | Data.Float r -> l + string r |> Data.String
        | Data.String r -> l + r |> Data.String
    |> Ok
let subData left right =
    match left, right with
    | _, Data.String _ ->  InvalidOperand (Sub,right) |> Error
    | Data.String _, _ -> InvalidOperand (Sub,right) |> Error
    | Data.Int l, Int r -> l - r |> Int |> Ok
    | Data.Int l,  Data.Float r -> float l - r |> Data.Float |> Ok
    | Data.Float l, Int r -> l - float r |> Data.Float |> Ok
    | Data.Float l, Data.Float r -> l - r |> Data.Float |> Ok
let mulData left right =
    match left, right with
    | _, Data.String _ ->  InvalidOperand (Sub,right) |> Error
    | Data.String _, _ -> InvalidOperand (Sub,right) |> Error
    | Int l, Int r -> l * r |> Int |> Ok
    | Int l,  Data.Float r -> float l * r |> Data.Float |> Ok
    | Data.Float l, Int r -> l * float r |> Data.Float |> Ok
    | Data.Float l, Data.Float r -> l * r |> Data.Float |> Ok
let divData left right =
    match left, right with
    | _, Data.String _ ->  InvalidOperand (Sub,right) |> Error
    | Data.String _, _ -> InvalidOperand (Sub,right) |> Error
    | Int num, _ | _, Int num when num = 0  ->
        DivisionByZero |> Error
    | Data.Float num, _ | _, Data.Float num when num = 0.0 ->
        DivisionByZero |> Error
    | Int l, Int r -> l / r |> Int |> Ok
    | Int l,  Data.Float r -> float l / r |> Data.Float |> Ok
    | Data.Float l, Int r -> l / float r |> Data.Float |> Ok
    | Data.Float l, Data.Float r -> l / r |> Data.Float |> Ok

let solveBinaryFieldExpr left op right =
    match op with
    | Add -> addData left right
    | Sub -> subData left right
    | Mul -> mulData left right
    | Div -> divData left right

let rec binaryFieldExprSolver expr =
    match expr with
    | Value data -> data |> Ok
    //| Column col -> failwith "not implemented"
    | BracedFieldExpr expr -> binaryFieldExprSolver expr
    | BinaryFieldExpr (left,op,right) ->
        result {
            let! left = binaryFieldExprSolver left
            let! right = binaryFieldExprSolver right
            return! solveBinaryFieldExpr left op right
        }
    | Column(_) -> Error CantSolveColumnRef 

let dataCastToFloat data =
    match data with
    | Int i -> float i |> Data.Float |> Ok
    | Data.Float _ -> data |> Ok
    | Data.String s ->
        let success, parsed = System.Double.TryParse s
        match success with
        | false -> InvalidLiteralCast(data,Data.Float 0.0) |> Error
        | true -> Data.Float parsed |> Ok

let dataCastToString data =
    match data with
    | Int i -> string i
    | Data.Float f -> string f
    | Data.String s -> s
    |> Data.String

let dataCastToInt data =
    match data with
    | Int _ -> data |> Ok
    | Data.Float f -> int f |> Int |> Ok
    | Data.String s ->
        let success, parsed = System.Int32.TryParse s
        match success with
        | false -> InvalidLiteralCast(data,Data.Int 0) |> Error
        | true -> Data.Int parsed |> Ok

let dataUpCastToCommonRepresentation left right =
    match left, right with
    | Data.String s, _ -> (left, dataCastToString right) |> Ok
    | _, Data.String s -> (dataCastToString left, right) |> Ok
    | Data.Float s, _ -> dataCastToFloat right |> Result.map (fun r -> (left,r))
    | _, Data.Float s -> dataCastToFloat left  |> Result.map (fun l -> (l,right))
    | _, _ -> (left, right) |> Ok
    


let rec boolExprSolver expr =
    match expr with
    | BoolLiteral(lit) -> lit |> Ok
    | BinaryBoolExpr(l,o,r) ->
        result {
            let! left = boolExprSolver l
            let! right = boolExprSolver r
            return match o with
                    | And -> left && right
                    | Or -> left || right
        }
    | RelationExpr(l, o, r) ->
        result {
            let! l = binaryFieldExprSolver l
            let! r = binaryFieldExprSolver r
            let! l,r = dataUpCastToCommonRepresentation l r
            return match o with
                    | Equals -> l = r
                    | Greater -> l > r
                    | GreaterOrEquals -> l >= r
                    | Smaller -> l < r
                    | SmallerOrEquals -> l <= r
                    | NotEquals -> l <> r
        }
    | Not(e) -> boolExprSolver e |> Result.map not
    | BracedBoolExpr(e) -> boolExprSolver e