module Insert

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
    NewInsertInput : string
    LastInsert : string
    Errors : APIError list
    Loading : bool
}

let init = {NewInsertInput = ""; Errors = []; Loading = false; LastInsert = ""}

type Msg =
    | InsertInputChange of string
    | Insert
    | InsertResult of Result<unit, APIError list>

let update insert (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    let doNothing = currentModel, Cmd.none
    match currentModel.Loading, msg with
    | true, InsertInputChange _ -> doNothing
    | false, InsertInputChange newInput -> {currentModel with NewInsertInput = newInput}, Cmd.none
    | true, Insert -> doNothing
    | false, Insert ->
        Cmd.OfAsync.start (insert currentModel.NewInsertInput)
        {currentModel with Loading = false}, Cmd.none
    | _, InsertResult (Ok _)->
        {currentModel with Loading = false; Errors = []; LastInsert = currentModel.NewInsertInput}, Cmd.none
    | _, InsertResult (Error errors) ->
        {currentModel with Errors = errors; Loading = false}, Cmd.none

let view (model : Model) lStr dispatch =
    let button =
        match model.Loading with
        | false ->
            Button.button [
                Button.Disabled false
                Button.IsLoading false
                Button.OnClick (fun event ->
                    event.preventDefault()
                    dispatch Insert)
                ]
                [lStr LStr.Save |> str]
        | true ->
            Button.button [
                Button.Disabled true
                Button.IsLoading true
                ]
                [lStr LStr.Save |> str]

    let InsertForm =
        Card.card [] [
            Card.header [] [
                Card.Header.title [] [lStr LStr.Insert |> str]
                Card.Header.icon [] [Icon.icon [] [Fa.i [Fa.Solid.SpellCheck] []]]
            ]
            Card.content [] [
                form [] [

                    textInput (lStr LStr.Insert) Text None (InsertInputChange >> dispatch)
                    pre [ ] [
                        (
                            match model.Errors with
                            | [] when model.LastInsert = "" -> lStr LStr.PleaseEnterAQuery |> str
                            | [] -> lStr LStr.Succes |> str
                            | errors ->
                                  errors
                                  |> List.map LStr.APIError
                                  |> errorMsgs lStr
                        )
                    ]
                    button
                ]
            ]
        ]
    div [] [
        InsertForm
    ]
