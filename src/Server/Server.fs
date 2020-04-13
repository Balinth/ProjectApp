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

open FSharp.Control.Tasks.V2
open Giraffe
open Shared

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open System.IdentityModel.Tokens.Jwt
open System.Security.Claims
open Microsoft.AspNetCore.Identity

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"
let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us


let secureAPI (user : UserInfo option) signOut = {
    logIn = fun () -> async {return user}
    logOut = fun () -> async {
        do! signOut()
        return user 
        }
    getUserDetails = fun () ->
        async {
            return user
        }
}


let securedAPI (ctx : HttpContext) =
    let subClaimType = ClaimsIdentityOptions().UserIdClaimType
    let sub = ctx.User.Claims.FirstOrDefault(fun c -> c.Type = subClaimType)
    let subValue = if isNotNull sub then Some sub.Value else None
    let user = {
        UserName = ctx.User.Identity.Name; 
        UserEmail = ctx.User.Claims |> Seq.map (fun c -> c.Type + "= " + c.Value + Environment.NewLine) |> Seq.fold (+) ""; 
        UserID = "userid not implemented" }
    secureAPI (Some user) (fun () -> Async.AwaitTask(ctx.SignOutAsync()) )

let authenticate : HttpHandler =
    challenge AzureADDefaults.AuthenticationScheme
    |> requiresAuthentication

let securedApp =
    authenticate >=> (
        Remoting.createApi()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.fromContext securedAPI
        |> Remoting.buildHttpHandler )

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
       .UseCors()
       .UseDefaultFiles()
       .UseStaticFiles()
       .UseCookiePolicy()
       .UseAuthentication()
       .UseSession()
       .UseGiraffe webApp

let configureServices (services : IServiceCollection) =
    let configuration = services.BuildServiceProvider().GetService<IConfiguration>()
    services
        .Configure( fun (opt:CookiePolicyOptions) ->
            opt.CheckConsentNeeded <- fun ctx -> true
            opt.MinimumSameSitePolicy <- SameSiteMode.None
            opt.HttpOnly <- CookiePolicy.HttpOnlyPolicy.Always
        )
        .Configure(AzureADDefaults.OpenIdScheme, fun (opt:OpenIdConnectOptions) ->
            opt.Authority <- opt.Authority + "/v2.0/"
            opt.Scope.Add("profile")
            opt.MaxAge <- Some (TimeSpan.FromSeconds 60.) |> Option.toNullable
            opt.TokenValidationParameters.ValidateIssuer <- false
        )
        .AddAuthentication(AzureADDefaults.AuthenticationScheme)
            .AddAzureAD(fun opt -> configuration.Bind("AzureAd", opt))
        |> ignore
    

    services
        .AddSession()
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
