module CommonView
open Fulma
open Fable.React.Helpers
open Fable.Core.JsInterop
open Fable.React.Standard

type InputType =
    | Text
    | Password
    | Email

let textInput inputLabel initial inputType (onChange : string -> unit) =
    let inputType =
        match inputType with
        | Text -> Input.text
        | Password -> Input.password
        | Email -> Input.email
    Field.div [] [
        Label.label [] [ str inputLabel ]
        Control.div [] [
            inputType [
                Input.Placeholder inputLabel
                Input.DefaultValue initial
                Input.OnChange (fun e -> onChange !!e.target?value)
            ]
        ]
    ]


let errorMsgs lstr errors =
    match errors with
    | [] -> div [] []
    | notEmptyErrors ->
        div [] [
            ul [] [
                for error in notEmptyErrors ->
                    Text.div [ Modifiers [Modifier.TextColor IsDanger] ] [
                        li [] [
                            error |> lstr |> str
                            ]
                    ]
            ]
        ]
