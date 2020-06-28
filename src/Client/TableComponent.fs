module TableComponent

open Elmish
open Shared

type Model = {
    Table : DynamicTable._T option
}

type Msg =
    | TableGot of DynamicTable._T option


let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel, msg with
    | _, TableGot table-> 
        {currentModel with Table = table}, Cmd.none
