module Query

open Elmish
open Shared
open DynamicTable
open DatabaseSchema
open PrimeReact.Column
open Fulma
open PrimeReact.DataTable
open Fable.React
open Fable.Core.JsInterop
open Language
open CommonView
open Fable.FontAwesome

type Model = {
    NewQueryInput : string
    LastQuery : string
    Errors : APIError list
    Table : DynamicTable<ProjectAppCol> option
    Loading : bool
}

let init = {NewQueryInput = ""; LastQuery = ""; Errors = []; Table = None; Loading = false}

type Msg =
    | QueryInputChange of string
    | Query
    | QueryResult of QueryResult
    | UnexpectedError of exn

let update query (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    let doNothing = currentModel, Cmd.none
    match currentModel.Loading, msg with
    | true, QueryInputChange _ -> doNothing
    | false, QueryInputChange newInput -> {currentModel with NewQueryInput = newInput}, Cmd.none
    | true, Query -> doNothing
    | false, Query ->
        {currentModel with Loading = true}, Cmd.OfAsync.either query currentModel.NewQueryInput QueryResult UnexpectedError
    | _, QueryResult (Ok newTable)->
        {currentModel with Table = Some newTable; Loading = false; LastQuery = currentModel.NewQueryInput}, Cmd.none
    | _, QueryResult (Error errors) ->
        {currentModel with Errors = errors}, Cmd.none
    | _, UnexpectedError err -> {currentModel with Loading = false}, Cmd.none //errorToast err


let tableDataFromDynTable (dynTable : DynamicTable.DynamicTable<ProjectAppCol>) =
    DynamicTable.rows dynTable
    |> List.map (fun row ->
        List.map2 (fun data header ->
            header ==>
                match data with
                | DynamicTable.Data.Int int -> string int
                | DynamicTable.Data.String str -> str
                | DynamicTable.Data.Float float -> string float) row.Data (DynamicTable.header dynTable |> List.map projectAppDBSchema.GetColumnName)
        |> createObj)
    |> ResizeArray


let tableHeader (dynTable : DynamicTable.DynamicTable<ProjectAppCol>) =
    let fields = List.map (projectAppDBSchema.GetColumnName >> ColProps.Field) (DynamicTable.header dynTable)
    let headers = List.map (projectAppDBSchema.GetColumnName >> ColProps.Header) (DynamicTable.header dynTable)
    List.map2 (fun field head -> PrimeReact.Column.ColBuilder [field; head; ColProps.Sortable true ]) fields headers
    |> Seq.ofList

let view (model : Model) lStr dispatch =

    let button =
        match model.Loading with
        | false ->
            Button.button [
                Button.Disabled false
                Button.IsLoading false
                Button.OnClick (fun event ->
                    event.preventDefault()
                    dispatch Query)
                ]
                [lStr LStr.Query |> str]
        | true ->
            Button.button [
                Button.Disabled true
                Button.IsLoading true
                ]
                [lStr LStr.Query |> str]
    let queryForm =
        Card.card [] [
            Card.header [] [
                Card.Header.title [] [lStr LStr.Query |> str]
                Card.Header.icon [] [Icon.icon [] [Fa.i [Fa.Solid.SpellCheck] []]]
            ]
            Card.content [] [
                form [] [

                    textInput (lStr LStr.Username) Text (QueryInputChange >> dispatch)
                    (
                      model.Errors
                      |> List.map LStr.APIError
                      |> errorMsgs lStr
                    )
                    button
                ]
            ]
        ]
    let table =
        match model.Table with
        | Some table ->
            let data = tableDataFromDynTable table
            Box.box' [] [
                DataTableBuilder [
                    DataTableProps.Header (Some ["Table View test"])
                    DataTableProps.Value (data)
                    ]
                    (tableHeader table)
            ]
        | None ->
            div [ ] [str "No table..."]
    div [] [
        queryForm
        table
    ]
