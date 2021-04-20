module Login

open Elmish
open Validation
open Shared
open Fulma
open Language
open CommonView
open Fable.React.Helpers
open Fable.React.Props
open Fable.React.Standard

type Msg =
    | UserNameInputChange of string
    | PasswordInputChange of string
    | Login
    | LoginResult of LoginResult

type LoginState =
    | WaitingForResponse
    | CanTry
    | Problems of userNameProblems:ValidationError list * passwordProblems:ValidationError list
    | LastLoginFailed

type Model = {
    UserNameInput: string
    PasswordInput: string
    LoginState: LoginState
}

let init = {UserNameInput = ""; PasswordInput = ""; LoginState = Problems ([],[])}

let update login loginSuccessMsg loginErrorMsg mapInnerMsg loginMsg loginModel =
    let login = login LoginResult
    match loginMsg with
    | UserNameInputChange newUsernameInput ->
        let newState =
            match checkUsernameAndPassword newUsernameInput loginModel.PasswordInput with
            | [], [] -> CanTry
            | someErrors -> Problems someErrors
        {loginModel with UserNameInput = newUsernameInput; LoginState = newState}, Cmd.none
    | PasswordInputChange newPasswordInput ->
        let newState =
            match checkUsernameAndPassword loginModel.UserNameInput newPasswordInput with
            | [], [] -> CanTry
            | someErrors -> Problems someErrors
        {loginModel with PasswordInput = newPasswordInput; LoginState = newState}, Cmd.none
    | Login ->
        match loginModel.LoginState with
        | CanTry ->
            let cmd = login {UserName= loginModel.UserNameInput; Password= loginModel.PasswordInput}
            {loginModel with LoginState = WaitingForResponse}, cmd |> Cmd.map mapInnerMsg
        | WaitingForResponse -> loginModel, Cmd.none
        | LastLoginFailed | Problems(_, _) -> loginModel, Cmd.none
    | LoginResult result ->
        match result with
        | Ok token ->
            loginModel, loginSuccessMsg token |> Cmd.ofMsg
        | Error loginError ->
                {loginModel with LoginState = LastLoginFailed}, loginErrorMsg loginError |> Cmd.ofMsg


let view (loginModel:Model) lstr dispatch =
    let button =
        match loginModel.LoginState with
        | LastLoginFailed ->
            Button.button [
                Button.Disabled true
                Button.IsLoading false
                Button.Color IsDanger
                ]
                [lstr LStr.Login |> str]
        | Problems _->
            Button.button [
                Button.Disabled true
                Button.IsLoading false
                ]
                [lstr LStr.Login |> str]
        | CanTry ->
            Button.button [
                Button.Disabled false
                Button.IsLoading false
                Button.OnClick (fun event ->
                    event.preventDefault()
                    dispatch Login)
                ]
                [lstr LStr.Login |> str]
        | WaitingForResponse ->
            Button.button [
                Button.Disabled true
                Button.IsLoading true
                ]
                [lstr LStr.Login |> str]

    form [] [
        Label.label [] [lstr LStr.Login |> str]
        textInput "Username" loginModel.UserNameInput Text (UserNameInputChange >> dispatch)
        (
          match loginModel.LoginState with
          | Problems (usernameProblems, _) ->
            List.map LStr.ValidationError usernameProblems
          | _ -> []
          |> errorMsgs lstr
        )
        textInput "Password" loginModel.PasswordInput Password (PasswordInputChange >> dispatch)
        (
          match loginModel.LoginState with
          | Problems (_, passwordProblems) ->
            List.map LStr.ValidationError passwordProblems
          | _ -> []
          |> errorMsgs lstr
        )
        button
    ]
