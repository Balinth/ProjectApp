module Login

open Elmish
open Validation
open Shared

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
