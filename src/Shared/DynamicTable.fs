module DynamicTable

type Data =
    | String of string
    | Int of int
    | Float of float
type Row = {
    Data : Data list
}

type Table<'c> = private {
    Header : 'c list
    Rows : Row list
}

module Table =

    let create header rows =
        if (rows |> List.filter (fun row -> row.Data.Length <> List.length header)).Length = 0
        then Some {Header = header; Rows = rows}
        else None