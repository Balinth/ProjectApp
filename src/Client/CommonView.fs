module CommonView
open Fulma
open Fable.React.Helpers
open Fable.Core.JsInterop
open Fable.React.Standard
open Fable.React.Props

type InputType =
    | Text
    | Password
    | Email

let textInput inputLabel inputType forcedInput (onChange : string -> unit) =
    let inputType =
        match inputType with
        | Text -> Input.text
        | Password -> Input.password
        | Email -> Input.email
    Field.div [] [
        Label.label [] [ str inputLabel ]
        Control.div [] [
            inputType (
                [
                Input.Placeholder inputLabel
                Input.OnChange (fun e -> onChange !!e.target?value)
            ] @ match forcedInput with
                | None -> []
                | Some input -> [Input.Value input]
            )
        ]
    ]


let errorMsgs lstr errors =
    match errors with
    | [] -> div [] []
    | notEmptyErrors ->
        div [ [WhiteSpaceOptions.PreWrap |> CSSProp.WhiteSpace] |> Style ] [
            ul [] [
                for error in notEmptyErrors ->
                    Text.div [ Modifiers [Modifier.TextColor IsDanger] ] [
                        li [] [
                            error |> lstr |> str
                            ]
                    ]
            ]
        ]
