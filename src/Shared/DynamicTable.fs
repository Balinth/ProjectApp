module DynamicTable

type Data =
    | String of string
    | Int of int
    | Float of float
type Row = {
    Data : Data list
}
// Note: never construct this directly!
type _T = {
    Header : string list
    Rows : Row list
}
let createTable header rows =
    if (rows |> List.filter (fun row -> row.Data.Length <> List.length header)).Length = 0
    then Some {Header = header; Rows = rows}
    else None