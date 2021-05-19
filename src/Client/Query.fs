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
open Fable.React.Props

type Model = {
    NewQueryInput : string
    LastQuery : string
    Errors : APIError list
    Table : DynamicTable<ProjectAppCol> option
    Loading : bool
    SavedQueryName : string option
    ForceInputValue : bool
}

let init = {NewQueryInput = ""; LastQuery = ""; Errors = []; Table = None; Loading = false; SavedQueryName = None; ForceInputValue = false}

type Msg =
    | QueryInputChange of string
    | ForcedQueryInputChange of string
    | Query
    | QueryResult of QueryResult
    | UnexpectedError of exn
    | QueryNameInputChange of string
    | SaveQueryStart
    | SaveQueryOk
    | SaveQueryCancel

let update query saveQuery (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    let doNothing = currentModel, Cmd.none
    let currentModel = {currentModel with ForceInputValue = false}
    match currentModel.Loading, msg with
    | true, QueryInputChange _ -> doNothing
    | false, QueryInputChange newInput -> {currentModel with NewQueryInput = newInput}, Cmd.none
    | _, ForcedQueryInputChange newInput -> {currentModel with NewQueryInput = newInput; ForceInputValue = true}, Cmd.none
    | true, Query -> doNothing
    | false, Query ->
        {currentModel with Loading = true}, Cmd.OfAsync.either query currentModel.NewQueryInput QueryResult UnexpectedError
    | _, QueryResult (Ok newTable)->
        {currentModel with Table = Some newTable; Loading = false; LastQuery = currentModel.NewQueryInput; Errors = []}, Cmd.none
    | _, QueryResult (Error errors) ->
        {currentModel with Errors = errors; Loading = false}, Cmd.none
    | _, UnexpectedError err -> {currentModel with Loading = false}, Cmd.none //errorToast err
    | _, SaveQueryStart -> {currentModel with SavedQueryName = Some ""}, Cmd.none
    | _, SaveQueryOk ->
        match currentModel.SavedQueryName with
        | Some name ->
            Cmd.OfAsync.start (saveQuery {Query = currentModel.NewQueryInput; Name = name; SavedBy = None})
            {currentModel with SavedQueryName = None}, Cmd.none
        | None -> doNothing
    | _, QueryNameInputChange newName ->
        match currentModel.SavedQueryName with
        | Some _ ->
            {currentModel with SavedQueryName = Some newName}, Cmd.none
        | None -> doNothing
    | _, SaveQueryCancel -> {currentModel with SavedQueryName = None}, Cmd.none


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
    let inputValue = (if model.ForceInputValue then Some model.NewQueryInput else None)
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
    let saveStartButton =
        match model.SavedQueryName with
        | None ->
            Button.button [
                    Button.OnClick (fun e ->
                        e.preventDefault()
                        dispatch SaveQueryStart)
            ] [lStr LStr.Save |> str ]
        | Some _ -> div [] []
    let saveQueryForm =
        match model.SavedQueryName with
        | None -> div [] []
        | Some queryName ->
            form [] [
                textInput (lStr LStr.QueryName) Text None (QueryNameInputChange >> dispatch)
                Button.button [
                    Button.OnClick (fun e ->
                        e.preventDefault()
                        dispatch SaveQueryOk)
                    Button.Disabled (queryName = "")
                ] [lStr LStr.Save |> str ]
                Button.button [
                    Button.OnClick (fun e ->
                        e.preventDefault()
                        dispatch SaveQueryCancel)
                    Button.IsInverted
                ] [lStr LStr.Cancel |> str ]
            ]

    let queryForm =
        Card.card [] [
            Card.header [] [
                Card.Header.title [] [lStr LStr.Query |> str]
                Card.Header.icon [] [Icon.icon [] [Fa.i [Fa.Solid.SpellCheck] []]]
            ]
            Card.content [] [
                form [] [

                    textInput (lStr LStr.Query) Text inputValue (QueryInputChange >> dispatch)
                    pre [ ] [
                        (
                            match model.Errors with
                            | [] when model.LastQuery = "" -> lStr LStr.PleaseEnterAQuery |> str
                            | [] -> lStr LStr.Succes |> str
                            | errors ->
                                  errors
                                  |> List.map LStr.APIError
                                  |> errorMsgs lStr
                        )
                    ]
                    button
                    saveStartButton
                ]
                saveQueryForm
            ]
        ]
    let table =
        match model.Table with
        | Some table ->
            let data = tableDataFromDynTable table
            Card.card [] [

                Card.header [] [
                    Card.Header.title [] [ lStr LStr.QueryResult |> str ]
                ]

                Card.content [] [
                    DataTableBuilder [
                        DataTableProps.Header (Some [model.LastQuery])
                        DataTableProps.Value (data)
                        DataTableProps.ResizableColumns (Some true)
                        ]
                        (tableHeader table)
                ]
            ]
        | None ->
            div [ ] [ ]
    div [] [
        queryForm
        table
    ]
