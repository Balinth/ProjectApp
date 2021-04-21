module Register

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
    | PasswordConfirmInputChange of string
    | EmailInputChange of string
    | GivenNameInputChange of string
    | FamilyNameInputChange of string
    | Register
    | RegisterResult of RegistrationResult

type RegisterState =
    | WaitingForResponse
    | CanTry
    | Problems of userNameProblems:ValidationError list * passwordProblems:ValidationError list * emailOk: bool * pwConfirmOk: bool
    | LastTryFailed of RegistrationError

type RegistrationFields = {
    UserNameInput: string
    PasswordInput: string
    PasswordConfirmInput: string
    GivenName : string
    FamilyName : string
    UserEmail : string
}

type Model = {
    RegistrationFields: RegistrationFields
    RegisterState: RegisterState
}

let init = {
    RegistrationFields = {
        UserNameInput = ""
        PasswordInput = ""
        PasswordConfirmInput = ""
        GivenName = ""
        FamilyName = ""
        UserEmail = ""
    }
    RegisterState = Problems ([],[], true, true)
    }

let checkRegistrationFields regFields =
    match Validation.checkRegisterInputs regFields.UserNameInput regFields.PasswordInput regFields.UserEmail regFields.PasswordConfirmInput with
    | [], [], true, true -> CanTry
    | problems -> Problems problems



let update register registerSuccessMsg registerErrorMsg mapInnerMsg registerMsg model =
    let register = register RegisterResult
    match registerMsg with
    | UserNameInputChange newUsernameInput ->
        let newInputs = {model.RegistrationFields with UserNameInput = newUsernameInput}
        {model with RegistrationFields = newInputs; RegisterState = checkRegistrationFields newInputs}, Cmd.none
    | PasswordConfirmInputChange newPasswordConfimInput ->
        let newInputs = {model.RegistrationFields with PasswordConfirmInput = newPasswordConfimInput}
        {model with RegistrationFields = newInputs; RegisterState = checkRegistrationFields newInputs}, Cmd.none
    | PasswordInputChange newPasswordInput ->
        let newInputs = {model.RegistrationFields with PasswordInput = newPasswordInput}
        {model with RegistrationFields = newInputs; RegisterState = checkRegistrationFields newInputs}, Cmd.none
    | EmailInputChange newEmail ->
        let newInputs = {model.RegistrationFields with UserEmail = newEmail}
        {model with RegistrationFields = newInputs; RegisterState = checkRegistrationFields newInputs}, Cmd.none
    | GivenNameInputChange newGivenName ->
        let newInputs = {model.RegistrationFields with GivenName = newGivenName}
        {model with RegistrationFields = newInputs}, Cmd.none
    | FamilyNameInputChange newFamilyName ->
        let newInputs = {model.RegistrationFields with FamilyName = newFamilyName}
        {model with RegistrationFields = newInputs}, Cmd.none
    | Register ->
        match model.RegisterState with
        | CanTry ->
            let cmd = register {
                UserName= model.RegistrationFields.UserNameInput
                Password= model.RegistrationFields.PasswordInput
                GivenName= model.RegistrationFields.GivenName
                FamilyName= model.RegistrationFields.FamilyName
                UserEmail= model.RegistrationFields.UserEmail
                }
            {model with RegisterState = WaitingForResponse}, cmd |> Cmd.map mapInnerMsg
        | WaitingForResponse -> model, Cmd.none
        | LastTryFailed errors -> model, Cmd.none
        | Problems _ -> model, Cmd.none
    | RegisterResult result ->
        match result with
        | Ok userName ->
            model, registerSuccessMsg userName |> Cmd.ofMsg
        | Error registerError ->
                {model with RegisterState = LastTryFailed registerError}, registerErrorMsg registerError |> Cmd.ofMsg


let view (model:Model) lstr dispatch =
    let password = lstr LStr.Password
    let passwordConfirm = lstr LStr.PasswordConfirm
    let username = lstr LStr.Username
    let givenname = lstr LStr.GivenName
    let familyname = lstr LStr.FamilyName
    let email = lstr LStr.Email

    let button =
        match model.RegisterState with
        | LastTryFailed _ ->
            Button.button [
                Button.Disabled true
                Button.IsLoading false
                Button.Color IsDanger
                ]
                [lstr LStr.Register |> str]
        | Problems _->
            Button.button [
                Button.Disabled true
                Button.IsLoading false
                ]
                [lstr LStr.Register |> str]
        | CanTry ->
            Button.button [
                Button.Disabled false
                Button.IsLoading false
                Button.OnClick (fun event ->
                    event.preventDefault()
                    dispatch Register)
                ]
                [lstr LStr.Register |> str]
        | WaitingForResponse ->
            Button.button [
                Button.Disabled true
                Button.IsLoading true
                ]
                [lstr LStr.Register |> str]

    form [] [
        Label.label [] [lstr LStr.Register |> str]
        textInput username model.RegistrationFields.UserNameInput Text (UserNameInputChange >> dispatch)
        (
          match model.RegisterState with
          | Problems (usernameProblems, _,_,_) ->
            List.map LStr.ValidationError usernameProblems
          | _ -> []
          |> errorMsgs lstr
        )
        textInput email model.RegistrationFields.UserEmail Email (EmailInputChange >> dispatch)
        (
          match model.RegisterState with
          | Problems (_, _,false,_) ->
            [LStr.InvalidEmail]
          | _ -> []
          |> errorMsgs lstr
        )
        textInput password model.RegistrationFields.PasswordInput Password (PasswordInputChange >> dispatch)
        (
          match model.RegisterState with
          | Problems (_, passwordProblems,_,_) ->
            List.map LStr.ValidationError passwordProblems
          | _ -> []
          |> errorMsgs lstr
        )
        textInput passwordConfirm model.RegistrationFields.PasswordConfirmInput Password (PasswordConfirmInputChange >> dispatch)
        (
          match model.RegisterState with
          | Problems (_, _,_,false) ->
            [LStr.ValidationError PasswordConfirmationDoesNotMatch]
          | _ -> []
          |> errorMsgs lstr
        )
        textInput givenname model.RegistrationFields.GivenName Text (GivenNameInputChange >> dispatch)
        textInput familyname model.RegistrationFields.FamilyName Text (FamilyNameInputChange >> dispatch)
        button
    ]
