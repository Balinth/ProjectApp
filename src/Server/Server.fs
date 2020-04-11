open System
open System.IO
open System.Threading.Tasks

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

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"
let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let counterApi = {
    initialCounter = fun () -> async { return { Value = 42 } }
}

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue counterApi
    |> Remoting.buildHttpHandler


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
    .Configure(Action<WebHostBuilderContext,IApplicationBuilder> configureApp)
    .ConfigureAppConfiguration(configureAppConfiguration)
    .ConfigureServices(configureServices)
    .UseUrls("https://0.0.0.0:" + port.ToString() + "/")
    .Build()
    .Run()
