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

JwtSecurityTokenHandler.DefaultMapInboundClaims <- false

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"
let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us


let secureAPI user signOut = {
    logIn = fun () -> async {return user}
    logOut = fun () -> async {
        do! signOut()
        return user 
        }
    getUserDetails = fun () ->
        async {
            return user
        }
    getTable = fun () -> async {
        return DynamicTable.createTable ["a"; "b"] [{Data= [DynamicTable.Data.String "1"; DynamicTable.Data.String "2"]}]
    }
}

let persistUser (user : UserInfo) =
    getUserResult user.UserNameID
    |> function
    | Ok dbUser when dbUser.Count() = 1 ->
        let dbUser = dbUser.First()
        Ok {UserInfo.UserName=dbUser.UserName; UserNameID=dbUser.UserNameID; UserEmail=dbUser.PrimaryEmail; GivenName=dbUser.GivenName; FamilyName=dbUser.FamilyName}
    | _ ->
        insertUser user 
        >>= (fun i -> if i = 1 then Ok user else Error [InsertFailed])

let getClaim (ctx:HttpContext) claimType =
    let claim = ctx.User.Claims.FirstOrDefault(fun c -> c.Type = claimType)
    match claim with
    | null -> [NoSuchClaim claimType] |> Error
    | nonNullClaim ->
        match nonNullClaim.Value with
        | null -> [ClaimHadNullValue claimType] |> Error
        | claimValue -> Ok claimValue

let securedAPI (ctx : HttpContext) =
    let sub = getClaim ctx "sub"
    let email = getClaim ctx "email"
    let familyName = getClaim ctx "family_name"
    let givenName = getClaim ctx "given_name"
    let userName = getClaim ctx "unique_name"

    let create userName sub email familyName givenName = {UserName=userName; GivenName=givenName; FamilyName=familyName; UserEmail=email; UserNameID=sub}

    let user =
        create
        <!> userName
        <*> sub
        <*> email
        <*> familyName
        <*> givenName

    let user2 =
            user
            |> Result.mapError (List.map ClaimError)
            >>= (persistUser >> Result.mapError (List.map DBError))

    secureAPI (user2) (fun () -> Async.AwaitTask(ctx.SignOutAsync()) )

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
