module SavedQueries

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
    SavedQueries : QueryInfo list option
}

let init = {SavedQueries = None}

type Msg =
    | GetSavedQueryList
    | ListSavedQueriesResult of Result<QueryInfo list,APIError list>
    | DeleteQuery of QueryInfo
    | ShowQuery of QueryInfo

let update listQueries deleteQuery (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    let doNothing = currentModel, Cmd.none
    match msg with
    | ListSavedQueriesResult (Ok result) -> {currentModel with SavedQueries = Some result}, Cmd.none
    | ListSavedQueriesResult _ -> doNothing
    | DeleteQuery q ->
        Cmd.OfAsync.start (deleteQuery q)
        {currentModel with SavedQueries = None}, Cmd.ofMsg GetSavedQueryList
    | ShowQuery _ -> doNothing
    | GetSavedQueryList -> currentModel, Cmd.OfAsync.perform listQueries () ListSavedQueriesResult


let view (model : Model) lStr dispatch =


    Card.card [] [
        Card.header [] [
            Card.Header.title [] [lStr LStr.ListSavedQueries |> str]
            Card.Header.icon [] [Icon.icon [] [Fa.i [Fa.Solid.Book] []]]
        ]
        Card.content [] [
            div [Class "card-table"] [
                Content.content [] [
                    match model.SavedQueries with
                    | None ->  Button.button [Button.IsLoading true] [str "..."]
                    | Some queries->
                        div [] [
                            Table.table [
                                Table.IsFullWidth
                                Table.IsStriped
                            ] [
                                tbody [] [
                                    for query in queries ->
                                        tr [] [
                                            td [Style [Width "5%"]] [
                                                Icon.icon [ Icon.Option.Props [OnClick (fun event ->
                                                                    event.preventDefault()
                                                                    dispatch (DeleteQuery query))]] [
                                                                        Fa.i [Fa.Solid.Trash] []
                                                                    ]
                                            ]
                                            td [] [str query.Name]
                                            td [] [str query.Query]
                                            td [Style [Width "5%"]] [
                                                Icon.icon [ Icon.Option.Props [OnClick (fun event ->
                                                                    event.preventDefault()
                                                                    dispatch (ShowQuery query))]] [
                                                                        Fa.i [Fa.Solid.ArrowRight] []
                                                                    ]
                                            ]
                                        ]
                                ]
                            ]
                        ]
                ]
            ]
        ]
    ]