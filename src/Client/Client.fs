module Client

open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json
open Fable.Core.JsInterop
open Fable.Remoting.Client

open ResultExtensions
open PrimeReact.Column
open PrimeReact.DataTable
open Shared
open Language
open Validation

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server

type UserModel = {
    User : UserInfo option
    Token : string
}

type QueryPage = {
    Table: TableComponent.Model
}

type SubPageModel =
    | UserPage
    | LoginPage of Login.Model
    | RegisterPage
    | QueryPage of QueryPage

type ChangePage =
    | ToUserPage
    | ToLoginPage
    | ToRegisterPage
    | ToQueryPage

type SubPageMsg =
    | LoginPageMsg of Login.Msg

type Model = {
    Language : Language.LStr -> string
    User : UserModel option
    SubPage : SubPageModel
}

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | ChangeLanguage of Language
    | ChangeToPage of ChangePage
    | SubPageMsg of SubPageMsg
    //| Login of LoginInfo
    | Logout
    | UserDetails of UserInfoResult
    //| APIErrors of APIError list
    | TableMsg of TableComponent.Msg
    | LoginSuccess of token:string
    | LoginFailed of LoginError
    | UnexpectedError of string


module Server =

    open Shared
    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api : ISecureAPI =
      Remoting.createApi()
      |> Remoting.withRouteBuilder Route.builder
      |> Remoting.buildProxy<ISecureAPI>

let loginAPI = Server.api.login
let register = Server.api.register
let getUser = Server.api.getUserDetails
//let getTable = Server.api.getTable

let secureRequestNaked fn token input =
    fn {Token=token;Body=input}

// Collapses the error cases from the secure request and the API itself into a unified error case
let secureRequest fn token input =
    async {
        let! result = secureRequestNaked fn token input
        return
            match result with
            | Ok (Ok result) -> Ok result
            | Ok (Error apiErr) -> Error apiErr
            | Error authErr -> Error authErr
    }
    //match  with
    //| Ok (Ok result) -> Ok result
    //| Ok (Error err) -> Error err
    //| Error err -> Error err

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { Language = getMLString English; User = None; SubPage = LoginPage Login.init}
    initialModel, Cmd.none

let cmdExnHandler toMsg exn =
        let err = sprintf "%A" exn
        printfn "%s" err
        toMsg err

let loginCmdExnHandler loginMsg exn =
    cmdExnHandler (UnexpectedLoginError >> Error >> loginMsg) exn

let getUserCmdExnHandler exn =
    cmdExnHandler (UnexpectedError) exn

let login loginMsg loginInfo =
    Cmd.OfAsync.either loginAPI loginInfo loginMsg (loginCmdExnHandler loginMsg)

let errorToast lng error : Cmd<_> =
    [fun _ -> printfn "%s" (lng error)]

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    let errorToastTmp = errorToast id
    let errorToast = errorToast currentModel.Language
    match msg with
    | ChangeLanguage l ->
        {currentModel with Language = Language.getMLString l}, Cmd.none
    //| Login loginInfo -> currentModel, Cmd.OfAsync.either login loginInfo LoginResult loginCmdExnHandler
    | UserDetails user ->
        match currentModel.User, user with
        | Some currentUser, Ok userInfo -> {currentModel with User = Some {currentUser with User = Some userInfo}}, Cmd.none
        | _, Error apiErrors ->
            currentModel,
            Cmd.batch (List.map (APIError >> errorToast) apiErrors)
        | _, Ok _ -> currentModel, errorToast (ClientError UserInfoWithoutToken)
    | TableMsg msg ->
        match currentModel.SubPage with
        | QueryPage queryPage ->
            let compUpdate = TableComponent.update msg queryPage.Table
            // for testing only
            let err = sprintf "TableMsg: %A" msg
            Fable.Core.JS.console.error err
            {currentModel with SubPage = QueryPage {queryPage with  Table = fst compUpdate }}, snd compUpdate |> Cmd.map TableMsg
        | _ -> currentModel, Cmd.none
    | ChangeToPage page ->
        match page, currentModel.User with
        | ToUserPage, Some _ -> {currentModel with SubPage=UserPage}, Cmd.none
        | ToUserPage, None -> {currentModel with SubPage=LoginPage Login.init}, Cmd.none
        | ToQueryPage, Some _ -> {currentModel with SubPage=QueryPage {Table = {Table = None}}}, Cmd.none
        | ToQueryPage, None -> {currentModel with SubPage=LoginPage Login.init}, Cmd.none
        | ToLoginPage, None -> {currentModel with SubPage=LoginPage Login.init}, Cmd.none
        | ToLoginPage, Some _ -> {currentModel with SubPage=LoginPage Login.init}, Cmd.ofMsg Logout
        | ToRegisterPage, None -> {currentModel with SubPage=RegisterPage}, Cmd.none
        | ToRegisterPage, Some _ -> {currentModel with SubPage=RegisterPage}, Cmd.ofMsg Logout
    | Logout -> {currentModel with User=None}, ToLoginPage |> ChangeToPage |> Cmd.ofMsg
    | SubPageMsg subPageMsg ->
            let newSubpageModel, cmds =
                match subPageMsg, currentModel.SubPage with
                | LoginPageMsg loginPageMsg, LoginPage loginPage ->
                    let model, (cmd:Cmd<Msg>) = Login.update login LoginSuccess LoginFailed (LoginPageMsg >> SubPageMsg) loginPageMsg loginPage
                    LoginPage model, cmd
                | LoginPageMsg _, someOtherSubpage -> someOtherSubpage, Cmd.none
            {currentModel with SubPage = newSubpageModel},cmds
    //| APIErrors(_) -> failwith "Not Implemented"
    | LoginSuccess token ->
        {currentModel with User = Some {User = None; Token = token }},
        Cmd.OfAsync.either (secureRequest getUser token) () UserDetails getUserCmdExnHandler
    | LoginFailed loginError ->
            currentModel, errorToast (LoginError loginError)
    | UnexpectedError err -> currentModel, errorToastTmp err

let tableDataFromDynTable (dynTable : DynamicTable._T) =
    dynTable.Rows
    |> List.map (fun row ->
        List.map2 (fun data header ->
            header ==>
                match data with
                | DynamicTable.Data.Int int -> string int
                | DynamicTable.Data.String str -> str
                | DynamicTable.Data.Float float -> string float) row.Data dynTable.Header
        |> createObj)
    |> ResizeArray


let tableHeader (dynTable : DynamicTable._T) =
    let fields = List.map ColProps.Field dynTable.Header
    let headers = List.map ColProps.Header dynTable.Header
    List.map2 (fun field head -> PrimeReact.Column.ColBuilder [field; head; ColProps.Sortable true ]) fields headers
    |> Seq.ofList

let tableView (model : QueryPage) dispatch =
    match model.Table.Table with
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


let safeComponents =
    let components =
        span [ ]
           [ a [ Href "https://github.com/SAFE-Stack/SAFE-template" ]
               [ str "SAFE FOS "
                 str Version.template ]
             str ", "
             a [ Href "https://github.com/giraffe-fsharp/Giraffe" ] [ str "Giraffe" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io" ] [ str "Elmish" ]
             str ", "
             a [ Href "https://fulma.github.io/Fulma" ] [ str "Fulma" ]
             str ", "
             a [ Href "https://bulmatemplates.github.io/bulma-templates/" ] [ str "Bulma\u00A0Templates" ]
             str ", "
             a [ Href "https://zaid-ajaj.github.io/Fable.Remoting/" ] [ str "Fable.Remoting" ]

           ]

    span [ ]
        [ str "Version "
          strong [ ] [ str Version.app ]
          str " powered by: "
          components ]

let showUser lString user =
    match user with
    | Some user -> user.UserName
    | None -> lString Language.Login

let navbarItemRaw dispatch str msg =
    Navbar.Item.a [ Navbar.Item.Option.Props [ OnClick(fun _ -> dispatch (msg)) ] ]
        [ str ]

let userPage (user:UserInfo) =
    str user.PrimaryEmail

let navBrand lStr (user:UserInfo option) dispatch =
    let navbarItem = fun s p -> (navbarItemRaw dispatch (str s) p)
    Navbar.navbar [ Navbar.Color IsWhite ]
        [ Container.container [ ]
            [ Navbar.Brand.div [ ]
                [ Navbar.Item.a [ Navbar.Item.CustomClass "brand-text" ]
                      [ str "BIM Admin" ] ]
              Navbar.menu [ ]
                  [ Navbar.Start.div [ ]
                      [
                        match user with
                        | Some user -> navbarItem user.UserName (ChangeToPage ToUserPage)
                        | None -> navbarItem (lStr LStr.Login) (ChangeToPage ToUserPage)

                        Navbar.Item.a [ ]
                            [ str "Orders" ]
                        Navbar.Item.a [ ]
                            [ str "Payments" ]
                        Navbar.Item.a [ ]
                            [ str "Exceptions" ] ] ] ] ]

let menu =
    Menu.menu [ ]
        [ Menu.label [ ]
              [ str "General" ]
          Menu.list [ ]
              [ Menu.Item.a [ ]
                    [ str "Dashboard" ]
                Menu.Item.a [ ]
                    [ str "Customers" ] ]
          Menu.label [ ]
              [ str "Administration" ]
          Menu.list [ ]
              [ Menu.Item.a [ ]
                  [ str "Team Settings" ]
                li [ ]
                    [ a [ ]
                        [ str "Manage Your Team" ]
                      Menu.list [ ]
                          [ Menu.Item.a [ ]
                                [ str "Members" ]
                            Menu.Item.a [ ]
                                [ str "Plugins" ]
                            Menu.Item.a [ ]
                                [ str "Add a member" ] ] ]
                Menu.Item.a [ ]
                    [ str "Invitations" ]
                Menu.Item.a [ ]
                    [ str "Cloud Storage Environment Settings" ]
                Menu.Item.a [ ]
                    [ str "Authentication" ] ]
          Menu.label [ ]
              [ str "Transactions" ]
          Menu.list [ ]
              [ Menu.Item.a [ ]
                    [ str "Payments" ]
                Menu.Item.a [ ]
                    [ str "Transfers" ]
                Menu.Item.a [ ]
                    [ str "Balance" ] ] ]

let breadcrump =
    Breadcrumb.breadcrumb [ ]
        [ Breadcrumb.item [ ]
              [ a [ ] [ str "Bulma" ] ]
          Breadcrumb.item [ ]
              [ a [ ] [ str "Templates" ] ]
          Breadcrumb.item [ ]
              [ a [ ] [ str "Examples" ] ]
          Breadcrumb.item [ Breadcrumb.Item.IsActive true ]
              [ a [ ] [ str "Admin" ] ] ]

let hero =
    Hero.hero [ Hero.Color IsInfo
                Hero.CustomClass "welcome" ]
        [ Hero.body [ ]
            [ Container.container [ ]
                [ Heading.h1 [ ]
                      [ str "Hello, Admin." ]
                  safeComponents ] ] ]

let info =
    section [ Class "info-tiles" ]
        [ Tile.ancestor [ Tile.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ Tile.parent [ ]
                  [ Tile.child [ ]
                      [ Box.box' [ ]
                          [ Heading.p [ ]
                                [ str "439k" ]
                            Heading.p [ Heading.IsSubtitle ]
                                [ str "Users" ] ] ] ]
              Tile.parent [ ]
                  [ Tile.child [ ]
                      [ Box.box' [ ]
                          [ Heading.p [ ]
                                [ str "59k" ]
                            Heading.p [ Heading.IsSubtitle ]
                                [ str "Products" ] ] ] ]
              Tile.parent [ ]
                  [ Tile.child [ ]
                      [ Box.box' [ ]
                          [ Heading.p [ ]
                                [ str "3.4k" ]
                            Heading.p [ Heading.IsSubtitle ]
                                [ str "Open Orders" ] ] ] ]
              Tile.parent [ ]
                  [ Tile.child [ ]
                      [ Box.box' [ ]
                          [ Heading.p [ ]
                                [ str "19" ]
                            Heading.p [ Heading.IsSubtitle ]
                                [ str "Exceptions" ] ] ] ] ] ]

let counter (model : Model) (dispatch : Msg -> unit) =
    Field.div [ Field.IsGrouped ]
        [ Control.p [ Control.IsExpanded ]
            [ Input.text
                [ Input.Disabled true
                  Input.Value "not implemented here" ] ]
          Control.p [ ]
            [ Button.a
                [ Button.Color IsInfo]
                [ str "+" ] ]
          Control.p [ ]
            [ Button.a
                [ Button.Color IsInfo]
                [ str "-" ] ] ]

type InputType =
    | Text
    | Password

let textInput inputLabel initial inputType (onChange : string -> unit) =
    let inputType =
        match inputType with
        | Text -> Input.text
        | Password -> Input.password
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

let columns (model : Model) (dispatch : Msg -> unit) =
    Columns.columns [ ]
        [ Column.column [ Column.Width (Screen.All, Column.Is6) ]
            [ Card.card [ CustomClass "events-card" ]
                [ Card.header [ ]
                    [ Card.Header.title [ ]
                        [ str "Events" ]
                      Card.Header.icon [ ]
                          [ Icon.icon [ ]
                              [ Fa.i [ Fa.Solid.AngleDown ] [] ] ] ]
                  div [ Class "card-table" ]
                      [ Content.content [ ]
                          [ Table.table
                              [ Table.IsFullWidth
                                Table.IsStriped ]
                              [ tbody [ ]
                                  [ for _ in 1..10 ->
                                      tr [ ]
                                          [ td [ Style [ Width "5%" ] ]
                                              [ Icon.icon
                                                  [ ]
                                                  [ Fa.i [ Fa.Regular.Bell ] [] ] ]
                                            td [ ]
                                                [ str "Lorem ipsum dolor aire" ]
                                            td [ ]
                                                [ Button.a
                                                    [ Button.Size IsSmall
                                                      Button.Color IsPrimary ]
                                                    [ str "Action" ] ] ] ] ] ] ]
                  Card.footer [ ]
                      [ Card.Footer.div [ ]
                          [ str "View All" ] ] ] ]
          Column.column [ Column.Width (Screen.All, Column.Is6) ]
              [ Card.card [ ]
                  [ Card.header [ ]
                      [ Card.Header.title [ ]
                          [ str "Inventory Search" ]
                        Card.Header.icon [ ]
                            [ Icon.icon [ ]
                                [ Fa.i [Fa.Solid.AngleDown] [] ] ] ]
                    Card.content [ ]
                        [ Content.content [ ]
                            [ Control.div
                                [ Control.HasIconLeft
                                  Control.HasIconRight ]
                                [ Input.text
                                      [ Input.Size IsLarge ]
                                  Icon.icon
                                      [ Icon.Size IsMedium
                                        Icon.IsLeft ]
                                      [ Fa.i [Fa.Solid.Search] [] ]
                                  Icon.icon
                                      [ Icon.Size IsMedium
                                        Icon.IsRight ]
                                      [ Fa.i [Fa.Solid.Check] [] ] ] ] ] ]
                Card.card [ ]
                    [ Card.header [ ]
                        [ Card.Header.title [ ]
                              [ str "Counter" ]
                          Card.Header.icon [ ]
                              [ Icon.icon [ ]
                                  [ Fa.i [Fa.Solid.AngleDown] [] ] ] ]
                      Card.content [ ]
                        [ Content.content   [ ]
                            [ counter model dispatch ] ] ]   ] ]

let userPageView lstr (user:UserInfo option) dispatch =
    match user with
    | None -> lstr LStr.ErrorNotLoggedIn |> str
    | Some user -> str user.UserName

let appIcon =
    img [
        Src "/img/icon.png"
        Style [
            Height 60
            Width 60
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

let loginPageView (loginModel:Login.Model) lstr dispatch =
    let button =
        match loginModel.LoginState with
        | Login.LastLoginFailed ->
            Button.button [
                Button.Disabled true
                Button.IsLoading false
                Button.Color IsDanger
                ]
                [lstr LStr.Login |> str]
        | Login.Problems _->
            Button.button [
                Button.Disabled true
                Button.IsLoading false
                ]
                [lstr LStr.Login |> str]
        | Login.CanTry ->
            Button.button [
                Button.Disabled false
                Button.IsLoading false
                Button.OnClick (fun _ -> dispatch Login.Login)
                ]
                [lstr LStr.Login |> str]
        | Login.WaitingForResponse ->
            Button.button [
                Button.Disabled true
                Button.IsLoading true
                ]
                [lstr LStr.Login |> str]

    div []
        [ div []
              [ div []
                    [ h1 [ Style [ TextAlign TextAlignOptions.Center ] ] [ str "BIM-BAM" ]
                      div [ Style [ TextAlign  TextAlignOptions.Center ] ] [ appIcon ]
                      br []
                      textInput "Username" loginModel.UserNameInput Text (Login.UserNameInputChange >> dispatch)
                      (
                          match loginModel.LoginState with
                          | Login.Problems (usernameProblems, _) ->
                            List.map LStr.ValidationError usernameProblems
                          | _ -> []
                          |> errorMsgs lstr
                      )
                      textInput "Password" loginModel.PasswordInput Password (Login.PasswordInputChange >> dispatch)
                      (
                          match loginModel.LoginState with
                          | Login.Problems (_, passwordProblems) ->
                            List.map LStr.ValidationError passwordProblems
                          | _ -> []
                          |> errorMsgs lstr
                      )

                      div [ Style [ TextAlign TextAlignOptions.Center ] ]
                          [
                            button
                          ]
                    ]
                ]
            ]



let subPageView model dispatch =
    match model.SubPage with
    | QueryPage queryPage -> tableView queryPage dispatch
    | UserPage -> userPageView model.Language ((model.User |> Option.bind (fun u -> u.User))) dispatch
    | LoginPage logPage -> loginPageView logPage model.Language (LoginPageMsg >> SubPageMsg >> dispatch)
    | RegisterPage -> failwith "Not Implemented"

let view (model : Model) (dispatch : Msg -> unit) =
    div [ ]
        [ navBrand model.Language (model.User |> Option.bind (fun u -> u.User)) dispatch
          Container.container [ ]
              [ Columns.columns [ ]
                  [ Column.column [ Column.Width (Screen.All, Column.Is3) ]
                      [ menu ]
                    Column.column [ Column.Width (Screen.All, Column.Is9) ]
                      [ breadcrump
                        hero
                        info
                        subPageView model dispatch
                        columns model dispatch
                        ] ] ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
