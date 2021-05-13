module DynamicTable

type Data =
    | String of string
    | Int of int
    | Float of float
type Row = {
    Data : Data list
}

type DynamicTable<'c> = {
    Header : 'c list
    Rows : Row list
}

let create header rows =
    if (rows |> List.filter (fun row -> row.Data.Length <> List.length header)).Length = 0
    then Some {Header = header; Rows = rows}
    else None
let rows table = table.Rows
let header table = table.Header