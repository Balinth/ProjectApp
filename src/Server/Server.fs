open System
open System.IO
open System.Threading.Tasks
open System.Linq

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authentication.AzureAD.UI
open Microsoft.AspNetCore.Authentication.OpenIdConnect
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Configuration
open System.IdentityModel.Tokens.Jwt
open System.Security.Claims
open Microsoft.AspNetCore.Identity

open FSharp.Control.Tasks.V2
open Giraffe
open Fable.Remoting.Server
open Fable.Remoting.Giraffe

open Shared
open DatabaseAccess
open ResultExtensions
open DynamicDBAccess

let liftAsync x = async { return x }


JwtSecurityTokenHandler.DefaultMapInboundClaims <- false

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x
let tryGetConfigOr<'a when 'a : null> (config: IConfiguration) key orDo =
    match config.GetValue<'a> key with
    | null -> orDo()
    | someStr -> someStr

let publicPath = Path.GetFullPath "../Client/public"
let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us
let serverSecretConfigKey = "ServerSecret"

let api : ISecureAPI = {
    register = register >> liftAsync
    login = login >> liftAsync
    getUserDetails = (Security.authorize userInfoFromAuthInfo)
    query = Security.authorize query
}

let securedApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromContext (fun ctx -> api)
    |> Remoting.buildHttpHandler

let webApp =
    choose [
        securedApp
    ]

let configureApp (context : WebHostBuilderContext) (app : IApplicationBuilder) =
    printfn "%s" context.HostingEnvironment.EnvironmentName
    if context.HostingEnvironment.EnvironmentName = "Development" then
        app.UseDeveloperExceptionPage() |> ignore
    else
        printfn "Note: Strict Transport Security header is used!"
        app.UseHsts() |> ignore
    app
       .UseHttpsRedirection()
       .UseHsts()
       .UseDefaultFiles()
       .UseStaticFiles()
       .UseGiraffe webApp

let configureServices (services : IServiceCollection) =
    let configuration = services.BuildServiceProvider().GetService<IConfiguration>()
    let serverSecret : string =
        tryGetConfigOr configuration serverSecretConfigKey
            (fun () -> failwith (sprintf "Could not find %s in configuration files!" serverSecretConfigKey ))
    System.Environment.SetEnvironmentVariable(serverSecretConfigKey, serverSecret)
    services
        .Configure( fun (opt:CookiePolicyOptions) ->
            opt.CheckConsentNeeded <- fun ctx -> true
            opt.MinimumSameSitePolicy <- SameSiteMode.None
            opt.HttpOnly <- CookiePolicy.HttpOnlyPolicy.Always
        )
        |> ignore
    services
        .AddGiraffe() |> ignore

let configureAppConfiguration  (context: WebHostBuilderContext) (config: IConfigurationBuilder) =
    config
        .AddJsonFile("appsettings.json",false,true)
        .AddJsonFile(sprintf "appsettings.%s.json" context.HostingEnvironment.EnvironmentName ,true)
        .AddEnvironmentVariables() |> ignore

WebHost
    .CreateDefaultBuilder()
    .UseKestrel()
    .UseWebRoot(publicPath)
    //.UseContentRoot(publicPath) // commented out because this seems stupid, and caused the config provider to look for appsettings.json in the public path folder....
    .ConfigureServices(configureServices)
    .Configure(Action<WebHostBuilderContext,IApplicationBuilder> configureApp)
    .ConfigureAppConfiguration(configureAppConfiguration)
    .UseUrls("https://0.0.0.0:" + port.ToString() + "/")
    .Build()
    .Run()
