module Client

open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json

open Shared
open Language
open Fable.Remoting.Client

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = { 
    Language : Language.LStr -> string
    User : UserInfo option
}

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | ChangeLanguage of Language
    | GetUser
    | UserDetails of UserInfo option

module Server =

    open Shared
    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api : ISecureAPI =
      Remoting.createApi()
      |> Remoting.withRouteBuilder Route.builder
      |> Remoting.buildProxy<ISecureAPI>

let logIn = Server.api.logIn
let logOut = Server.api.logOut
let getUser = Server.api.getUserDetails

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { Language = getMLString English; User = None }
    initialModel, Cmd.ofMsg GetUser


let signIn =
    let res = fun () -> Fetch.fetch @"/api/ISecureAPI/logInFOS" [
        Fetch.Types.RequestProperties.Method Fetch.Types.HttpMethod.GET
        Fetch.Types.RequestProperties.Mode Fetch.Types.RequestMode.Nocors
        Fetch.requestHeaders [
            Fetch.Types.Custom ("Sec-Fetch-Site", "none")
            Fetch.Types.Custom ("fisfos", "none")
            
            ]
        ]
    res

let unpackResp (resp : Fetch.Types.Response) =
    Some {UserName = string resp.Ok; UserID = ""; UserEmail = ""} |> UserDetails
    
let userDetailResult userDetails =
    match userDetails with
    | Ok user -> Some user |> UserDetails
    | Error err ->
        printfn "%A" err
        UserDetails None

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    | ChangeLanguage l -> {currentModel with Language = Language.getMLString l}, Cmd.none
    //| SignIn -> currentModel, Cmd.OfAsync.perform logIn () SignInResult
    //| GetUser -> currentModel, Cmd.OfPromise.perform signIn () unpackResp
    | GetUser -> currentModel, Cmd.OfAsync.either getUser () userDetailResult (fun exn ->
        printfn "Error getting user info: %A" exn 
        UserDetails None)
    | UserDetails user -> {currentModel with User = user}, Cmd.none


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

let navBrand lStr user dispatch=
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
                        | Some user -> navbarItem user.UserName GetUser
                        | None -> navbarItem (lStr Login) GetUser

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

let view (model : Model) (dispatch : Msg -> unit) =
    div [ ]
        [ navBrand model.Language model.User dispatch
          Container.container [ ]
              [ Columns.columns [ ]
                  [ Column.column [ Column.Width (Screen.All, Column.Is3) ]
                      [ menu ]
                    Column.column [ Column.Width (Screen.All, Column.Is9) ]
                      [ breadcrump
                        hero
                        info
                        columns model dispatch ] ] ] ]

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
