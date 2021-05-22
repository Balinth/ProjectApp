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
open DatabaseSchema
open DynamicTable

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server

type UserModel = {
    User : UserInfo
    Token : Token
}

type SubPageModel =
    | UserPage
    | LoginPage of Login.Model
    | RegisterPage of Register.Model
    | QueryPage of Query.Model
    | InsertPage of Insert.Model
    | SavedQueriesPage of SavedQueries.Model

type ChangePage =
    | ToUserPage
    | ToLoginPage
    | ToRegisterPage
    | ToQueryPage
    | ToInsertPage
    | ToSavedQueriesPage

type SubPageMsg =
    | LoginPageMsg of Login.Msg
    | RegisterPageMsg of Register.Msg
    | QueryPageMsg of Query.Msg
    | InsertPageMsg of Insert.Msg
    | SavedQueriesPageMsg of SavedQueries.Msg

type Model = {
    Language : Language//.LStr -> string
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
let queryAPI = Server.api.query
let saveQueryAPI = Server.api.saveQuery
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

let secureRequestSimpleIgnore fn token input =
    async {
        let! result = secureRequestNaked fn token input
        return ignore result
    }

let secureRequestSimple fn token input =
    async {
        let! result = secureRequestNaked fn token input
        return result
    }



// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { Language = English; User = None; SubPage = RegisterPage Register.init}
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
    let errorToast = errorToast (getMLString currentModel.Language)
    match msg with
    | ChangeLanguage l ->
        {currentModel with Language = l}, Cmd.none
    | ChangePage page ->
        match page, currentModel.User with
        | ToUserPage, Some _ -> {currentModel with SubPage=UserPage}, Cmd.none
        | ToUserPage, None -> {currentModel with SubPage=LoginPage Login.init}, Cmd.none
        | ToQueryPage, Some _ -> {currentModel with SubPage=QueryPage Query.init}, Cmd.none
        | ToQueryPage, None -> {currentModel with SubPage=LoginPage Login.init}, Cmd.none
        | ToInsertPage, Some _ -> {currentModel with SubPage=InsertPage Insert.init}, Cmd.none
        | ToInsertPage, None -> {currentModel with SubPage=LoginPage Login.init}, Cmd.none
        | ToSavedQueriesPage, Some _ ->
            {currentModel with SubPage=SavedQueriesPage SavedQueries.init},
            Cmd.ofMsg (SavedQueries.GetSavedQueryList |> SavedQueriesPageMsg |> SubPageMsg)
        | ToSavedQueriesPage, None -> {currentModel with SubPage=LoginPage Login.init}, Cmd.none
        | ToLoginPage, None -> {currentModel with SubPage=LoginPage Login.init}, Cmd.none
        | ToLoginPage, Some _ -> {currentModel with SubPage=LoginPage Login.init}, Cmd.ofMsg Logout
        | ToRegisterPage, None -> {currentModel with SubPage=RegisterPage Register.init}, Cmd.none
        | ToRegisterPage, Some _ -> {currentModel with SubPage=RegisterPage Register.init}, Cmd.ofMsg Logout
    | Logout -> {currentModel with User=None}, ToLoginPage |> ChangePage |> Cmd.ofMsg
    | SubPageMsg subPageMsg ->
            let newSubpageModel, cmds =
                match subPageMsg, currentModel.SubPage, currentModel.User with
                | LoginPageMsg loginPageMsg, LoginPage loginPage, _ ->
                    let model, (cmd:Cmd<Msg>) = Login.update login LoginSuccess LoginFailed (LoginPageMsg >> SubPageMsg) loginPageMsg loginPage
                    LoginPage model, cmd
                | LoginPageMsg _, someOtherSubpage, _ -> someOtherSubpage, Cmd.none
                | RegisterPageMsg registerPageMsg, RegisterPage registerPage, _ ->
                    let model, cmd = Register.update register RegistrationSuccess RegistrationFailed (RegisterPageMsg >> SubPageMsg) registerPageMsg registerPage
                    RegisterPage model, cmd
                | RegisterPageMsg _, someOtherSubpage, _ -> someOtherSubpage, Cmd.none
                | QueryPageMsg _, QueryPage currentModel, None -> QueryPage currentModel, Cmd.ofMsg (ChangePage ToLoginPage)
                | QueryPageMsg queryMsg, QueryPage queryPage, Some user ->
                    let query = secureRequest queryAPI user.Token
                    let saveQuery = secureRequestSimpleIgnore saveQueryAPI user.Token
                    let model, cmd = Query.update query saveQuery queryMsg queryPage
                    // for testing only
                    //let err = sprintf "TableMsg: %A" msg
                    //Fable.Core.JS.console.error err
                    QueryPage model, cmd |> Cmd.map (QueryPageMsg >> SubPageMsg)
                    //QueryPage queryPage, Cmd.ofMsg (ChangePage ToLoginPage)
                | QueryPageMsg _, someOtherSubpage, _ -> someOtherSubpage, Cmd.none
                | InsertPageMsg insertMsg, InsertPage insertPage, Some user ->
                    let insert = secureRequestSimpleIgnore Server.api.insert user.Token
                    let model, cmd = Insert.update insert insertMsg insertPage
                    InsertPage model, cmd |> Cmd.map (InsertPageMsg >> SubPageMsg)
                | InsertPageMsg _, someOtherSubpage, _ -> someOtherSubpage, Cmd.none
                | SavedQueriesPageMsg (SavedQueries.ShowQuery(query)), SavedQueriesPage savedQueriesPage, Some user ->
                    SavedQueriesPage savedQueriesPage, Cmd.batch [Cmd.ofMsg(ChangePage ToQueryPage); Cmd.ofMsg(Query.ForcedQueryInputChange query.Query |> QueryPageMsg |> SubPageMsg)]
                | SavedQueriesPageMsg savedQueryMsg, SavedQueriesPage savedQueriesPage, Some user ->
                    let listSavedQueries = secureRequestSimple Server.api.listSavedQueries user.Token
                    let deleteQuery = secureRequestSimpleIgnore Server.api.deleteQuery user.Token
                    let model, cmd = SavedQueries.update listSavedQueries deleteQuery savedQueryMsg savedQueriesPage
                    SavedQueriesPage model, cmd |> Cmd.map (SavedQueriesPageMsg >> SubPageMsg)
                | SavedQueriesPageMsg _, someOtherSubpage, _ -> someOtherSubpage, Cmd.none
            {currentModel with SubPage = newSubpageModel},cmds
    //| APIErrors(_) -> failwith "Not Implemented"
    | LoginSuccess (token, user) ->
        {currentModel with User = Some {User = user; Token = token }},
        Cmd.ofMsg (ChangePage ToQueryPage)
    | LoginFailed loginError ->
            currentModel, errorToast (LoginError loginError)
    | UnexpectedError err -> currentModel, errorToastTmp err
    | RegistrationSuccess(_) -> currentModel, Cmd.ofMsg (ChangePage ToLoginPage)
    | RegistrationFailed error ->
        currentModel ,Cmd.batch [
            errorToast (LStr.RegistrationError error)
        ]

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

let navbarItemRaw dispatch str icon msg =
    Navbar.Item.a [ Navbar.Item.Option.Props [ OnClick(fun _ -> dispatch (msg)) ] ]
        [ str; icon ]

let navbarItemRawSimple dispatch str msg =
    Navbar.Item.a [ Navbar.Item.Option.Props [ OnClick(fun _ -> dispatch (msg)) ] ]
        [ str ]

let userPage (user:UserInfo) =
    str user.PrimaryEmail

let navBrand currentLang (user:UserInfo option) dispatch =
    let lStr = getMLString currentLang
    let languageSwitch =
        match currentLang with
        | English -> "Hungarian", ChangeLanguage Hungarian
        | Hungarian -> "English", ChangeLanguage English
    let navbarItem = fun s p -> (navbarItemRawSimple dispatch (str s) p)
    let navbarItemIcon = fun s icon p -> (navbarItemRaw dispatch (str s) icon p)
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
                                    navbarItem (lStr LStr.Query) (ChangePage ToQueryPage)
                                    navbarItem (lStr LStr.Insert) (ChangePage ToInsertPage)
                                    navbarItem (lStr LStr.ListSavedQueries) (ChangePage ToSavedQueriesPage)
                                    navbarItem (lStr LStr.Logout) (Logout)
                                ]
                            | None -> [
                                    navbarItem (lStr LStr.Login) (ChangePage ToUserPage)
                                    navbarItem (lStr LStr.Register) (ChangePage ToRegisterPage)
                                ]
                            );
                            [
                               // Navbar.Item.a [ ]
                               //     [ str "Orders" ]
                               // Navbar.Item.a [ ]
                               //     [ str "Payments" ]
                            ]
                        ])
                    Navbar.End.div [] [
                        navbarItemIcon (fst languageSwitch) (Icon.icon [ ] [ Fa.i [ Fa.Solid.Globe ] [] ]) (snd languageSwitch)
                    ]
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

let hero (user:UserInfo) =
    Hero.hero [ Hero.Color IsInfo
                Hero.CustomClass "welcome" ]
        [ Hero.body [ ]
            [ Container.container [ ]
                [ Heading.h1 [ ]
                      [ str ("Hello, " + user.FamilyName + ".") ]
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
    let lStr = getMLString model.Language
    match model.SubPage with
    | QueryPage queryPage -> Query.view queryPage lStr (QueryPageMsg >> SubPageMsg >> dispatch)
    | UserPage -> UserDetails.view lStr ((model.User |> Option.map (fun u -> u.User))) dispatch
    | LoginPage logPage -> Login.view logPage lStr (LoginPageMsg >> SubPageMsg >> dispatch)
    | RegisterPage regPage -> Register.view regPage lStr (RegisterPageMsg >> SubPageMsg >> dispatch)
    | InsertPage insertPage -> Insert.view insertPage lStr (InsertPageMsg >> SubPageMsg >> dispatch)
    | SavedQueriesPage savedQueriesPage -> SavedQueries.view savedQueriesPage lStr (SavedQueriesPageMsg >> SubPageMsg >> dispatch)

let view (model : Model) (dispatch : Msg -> unit) =
    div [ ]
        [ navBrand model.Language (model.User |> Option.map (fun u -> u.User)) dispatch
          Container.container [ ]
              [ Columns.columns [ ]
                  [ //Column.column [ Column.Width (Screen.All, Column.Is3) ]
                      //[]//[ menu ]
                    Column.column [ Column.Width (Screen.All, Column.Is9) ]
                      [ //breadcrump model.SubPage
                        match model.User with
                        | Some user -> hero user.User
                        | None -> div [] []
                        //info
                        subPageView model dispatch
                        //columns model dispatch
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
