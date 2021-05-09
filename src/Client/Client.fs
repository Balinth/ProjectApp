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
    User : UserInfo
    Token : Token
}

type QueryPage = {
    Table: TableComponent.Model
}

type SubPageModel =
    | UserPage
    | LoginPage of Login.Model
    | RegisterPage of Register.Model
    | QueryPage of QueryPage

type ChangePage =
    | ToUserPage
    | ToLoginPage
    | ToRegisterPage
    | ToQueryPage

type SubPageMsg =
    | LoginPageMsg of Login.Msg
    | RegisterPageMsg of Register.Msg

type Model = {
    Language : Language.LStr -> string
    User : UserModel option
    SubPage : SubPageModel
}

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | ChangeLanguage of Language
    | ChangePage of ChangePage
    | SubPageMsg of SubPageMsg
    | Logout
    | TableMsg of TableComponent.Msg
    | LoginSuccess of token:Token * user: UserInfo
    | LoginFailed of LoginError
    | RegistrationSuccess of UserName
    | RegistrationFailed of RegistrationError
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
let registerAPI = Server.api.register
let getUser = Server.api.getUserDetails
//let getTable = Server.api.getTable

let secureRequestNaked fn token input =
    fn {Token=token;Body=input}

// Collapses the error cases from the secure request and the API itself into a unified error case
let secureRequest fn token input =
    let (Token(token)) = token
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
    let initialModel = { Language = getMLString English; User = None; SubPage = RegisterPage Register.init}
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

let register registerMsg registerInfo =
    Cmd.OfAsync.either registerAPI registerInfo registerMsg (cmdExnHandler (UnexpectedRegistrationError >> Error >> registerMsg))

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
    | TableMsg msg ->
        match currentModel.SubPage with
        | QueryPage queryPage ->
            let compUpdate = TableComponent.update msg queryPage.Table
            // for testing only
            let err = sprintf "TableMsg: %A" msg
            Fable.Core.JS.console.error err
            {currentModel with SubPage = QueryPage {queryPage with  Table = fst compUpdate }}, snd compUpdate |> Cmd.map TableMsg
        | _ -> currentModel, Cmd.none
    | ChangePage page ->
        match page, currentModel.User with
        | ToUserPage, Some _ -> {currentModel with SubPage=UserPage}, Cmd.none
        | ToUserPage, None -> {currentModel with SubPage=LoginPage Login.init}, Cmd.none
        | ToQueryPage, Some _ -> {currentModel with SubPage=QueryPage {Table = {Table = None}}}, Cmd.none
        | ToQueryPage, None -> {currentModel with SubPage=LoginPage Login.init}, Cmd.none
        | ToLoginPage, None -> {currentModel with SubPage=LoginPage Login.init}, Cmd.none
        | ToLoginPage, Some _ -> {currentModel with SubPage=LoginPage Login.init}, Cmd.ofMsg Logout
        | ToRegisterPage, None -> {currentModel with SubPage=RegisterPage Register.init}, Cmd.none
        | ToRegisterPage, Some _ -> {currentModel with SubPage=RegisterPage Register.init}, Cmd.ofMsg Logout
    | Logout -> {currentModel with User=None}, ToLoginPage |> ChangePage |> Cmd.ofMsg
    | SubPageMsg subPageMsg ->
            let newSubpageModel, cmds =
                match subPageMsg, currentModel.SubPage with
                | LoginPageMsg loginPageMsg, LoginPage loginPage ->
                    let model, (cmd:Cmd<Msg>) = Login.update login LoginSuccess LoginFailed (LoginPageMsg >> SubPageMsg) loginPageMsg loginPage
                    LoginPage model, cmd
                | LoginPageMsg _, someOtherSubpage -> someOtherSubpage, Cmd.none
                | RegisterPageMsg registerPageMsg, RegisterPage registerPage ->
                    let model, cmd = Register.update register RegistrationSuccess RegistrationFailed (RegisterPageMsg >> SubPageMsg) registerPageMsg registerPage
                    RegisterPage model, cmd
                | RegisterPageMsg _, someOtherSubpage -> someOtherSubpage, Cmd.none
            {currentModel with SubPage = newSubpageModel},cmds
    //| APIErrors(_) -> failwith "Not Implemented"
    | LoginSuccess (token, user) ->
        {currentModel with User = Some {User = user; Token = token }},
        Cmd.ofMsg (ChangePage ToUserPage)
    | LoginFailed loginError ->
            currentModel, errorToast (LoginError loginError)
    | UnexpectedError err -> currentModel, errorToastTmp err
    | RegistrationSuccess(_) -> currentModel, Cmd.ofMsg (ChangePage ToLoginPage)
    | RegistrationFailed error ->
        currentModel ,Cmd.batch [
            errorToast (LStr.RegistrationError error)
        ]

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
                        (List.concat [
                            (match user with
                            | Some user -> [
                                    navbarItem user.UserName (ChangePage ToUserPage)
                                    navbarItem (lStr LStr.Logout) (Logout)
                                ]
                            | None -> [
                                    navbarItem (lStr LStr.Login) (ChangePage ToUserPage)
                                    navbarItem (lStr LStr.Register) (ChangePage ToRegisterPage)
                                ]
                            );
                            [
                                Navbar.Item.a [ ]
                                    [ str "Orders" ]
                                Navbar.Item.a [ ]
                                    [ str "Payments" ]
                                Navbar.Item.a [ ]
                                    [ str "Exceptions" ]
                            ]
                        ])
                   ]
                ]
            ]

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

let subPageView model dispatch =
    match model.SubPage with
    | QueryPage queryPage -> tableView queryPage dispatch
    | UserPage -> UserDetails.view model.Language ((model.User |> Option.map (fun u -> u.User))) dispatch
    | LoginPage logPage -> Login.view logPage model.Language (LoginPageMsg >> SubPageMsg >> dispatch)
    | RegisterPage regPage -> Register.view regPage model.Language (RegisterPageMsg >> SubPageMsg >> dispatch)

let view (model : Model) (dispatch : Msg -> unit) =
    div [ ]
        [ navBrand model.Language (model.User |> Option.map (fun u -> u.User)) dispatch
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
