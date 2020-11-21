open Fake.Core
#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"

#if !FAKE
#r "netstandard"
#r "Facades/netstandard" // https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095
#endif

open System
open System.IO

open Fake.Core
open Fake.DotNet
open Fake.IO

open Newtonsoft.Json.Linq

Target.initEnvironment ()

let serverPath = Path.getFullName "./src/Server"
let serverTestsPath = Path.getFullName "./src/ServerTests"
let clientPath = Path.getFullName "./src/Client"
let clientDeployPath = Path.combine clientPath "deploy"
let deployDir = Path.getFullName "./deploy"

let dbPath = Path.getFullName "./db" // Note: you must place the sqlite3.exe in this path
let devDBFile = "DevDatabase.db"
// obtain secret for tls cert
let appsettings = JObject.Parse(File.readAsString( serverPath + @"/appsettings.json"))
let certPath = serverPath + (string appsettings.["ASPNETCORE_Kestrel__Certificates__Default__Path"] )
let pwSecret = string appsettings.["ASPNETCORE_Kestrel__Certificates__Default__Password"]
TraceSecrets.register "<SECRET>" pwSecret

let release = ReleaseNotes.load "RELEASE_NOTES.md"

let envTest = Environment.environVarAsBool "FAKE_DEBUG_PROCESS_HANG"

let platformTool tool winTool =
    let tool = if Environment.isUnix then tool else winTool
    match ProcessUtils.tryFindFileOnPath tool with
    | Some t -> t
    | _ ->
        let errorMsg =
            tool + " was not found in path. " +
            "Please install it and make sure it's available from your path. " +
            "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
        failwith errorMsg

let nodeTool = platformTool "node" "node.exe"
let yarnTool = platformTool "yarn" "yarn.cmd"

let sqliteTool = platformTool "sqlite3" (dbPath + "/sqlite3.exe")

let runTool cmd args workingDir =
    let arguments = args |> String.split ' ' |> Arguments.OfArgs
    Command.RawCommand (cmd, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let runToolWithInput cmd args workingDir inputStream =
    let arguments = args |> String.split ' ' |> Arguments.OfArgs
    Command.RawCommand (cmd,arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> CreateProcess.withStandardInput (CreatePipe inputStream)
    |> Proc.start
    |> ignore

let runToolWithOutputFiltering cmd args workingDir = 
    let arguments = args |> String.split ' ' |> Arguments.OfArgs
    Command.RawCommand (cmd, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> CreateProcess.redirectOutput
    |> CreateProcess.withOutputEventsNotNull Trace.trace Trace.traceError
    |> Proc.run
    |> ignore

let runDotNet cmd workingDir =
    let result =
        DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

let openBrowser url =
    //https://github.com/dotnet/corefx/issues/10361
    Command.ShellCommand url
    |> CreateProcess.fromCommand
    |> CreateProcess.ensureExitCodeWithMessage "opening browser failed"
    |> Proc.run
    |> ignore


Target.create "Clean" (fun _ ->
    [ deployDir
      clientDeployPath ]
    |> Shell.cleanDirs
)

let devDBFun = (fun _ ->
    let input = ".read DevDatabase.sql" + Environment.NewLine
    let streamRef = StreamRef.Empty
    Fake.IO.Shell.copyFile dbPath (serverPath + "/DevDatabase.sql")
    runToolWithInput sqliteTool devDBFile dbPath streamRef
    use writer = new StreamWriter(streamRef.Value)
    writer.Write(input)
    writer.Flush()
    writer.Write(".exit")
    writer.Flush()
    Trace.trace "Initialized DB"
)

Target.create "DevDB" devDBFun
Target.create "ResetDB" devDBFun

Target.create "DeleteDB" (fun _ ->
    File.Delete(Path.Combine(dbPath, devDBFile))
)

Target.create "InstallClient" (fun _ ->
    printfn "Node version:"
    runTool nodeTool "--version" __SOURCE_DIRECTORY__
    printfn "Yarn version:"
    runTool yarnTool "--version" __SOURCE_DIRECTORY__
    runTool yarnTool "install --frozen-lockfile" __SOURCE_DIRECTORY__
)

Target.create "Build" (fun _ ->
    runDotNet "build" serverPath
    runDotNet "build" serverTestsPath
    Shell.regexReplaceInFileWithEncoding
        "let app = \".+\""
       ("let app = \"" + release.NugetVersion + "\"")
        System.Text.Encoding.UTF8
        (Path.combine clientPath "Version.fs")
    runTool yarnTool "webpack-cli -p" __SOURCE_DIRECTORY__
)

Target.create "TestServer" <| fun _ ->
    runDotNet "run" serverTestsPath

Target.create "Run" (fun _ ->
    let server = async {
        runDotNet "watch run" serverPath
    }
    let client = async {
        runToolWithOutputFiltering yarnTool ("webpack-dev-server" + " --https --pfx=" + certPath + " --pfx-passphrase=" + pwSecret) __SOURCE_DIRECTORY__
    }
    let browser = async {
        do! Async.Sleep 5000
        openBrowser "https://localhost:8080"
    }

    let vsCodeSession = Environment.hasEnvironVar "vsCodeSession"
    let safeClientOnly = Environment.hasEnvironVar "safeClientOnly"

    let tasks =
        [ if not safeClientOnly then yield server
          yield client
          if not vsCodeSession then yield browser ]

    tasks
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
)

let buildDocker tag =
    let args = sprintf "build -t %s ." tag
    runTool "docker" args __SOURCE_DIRECTORY__

Target.create "Bundle" (fun _ ->
    let serverDir = Path.combine deployDir "Server"
    let clientDir = Path.combine deployDir "Client"
    let publicDir = Path.combine clientDir "public"

    let publishArgs = sprintf "publish -c Release -o \"%s\"" serverDir
    runDotNet publishArgs serverPath

    Shell.copyDir publicDir clientDeployPath FileFilter.allFiles
)

let dockerUser = "safe-template"
let dockerImageName = "safe-template"
let dockerFullName = sprintf "%s/%s" dockerUser dockerImageName

Target.create "Docker" (fun _ ->
    buildDocker dockerFullName
)

open Fake.Core.TargetOperators

"Clean"
    ==> "InstallClient"
    ==> "Build"
    ==> "TestServer"
    ==> "Bundle"
    ==> "Docker"


"Clean"
    ==> "DevDB"
    ==> "InstallClient"
    ==> "Run"

"Clean" ==> "DeleteDB" ==> "ResetDB"

"DeleteDB" ?=> "DevDB"

"Clean" ==> "DevDB"

Target.runOrDefaultWithArguments "Build"