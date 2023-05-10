module Program

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Xml
open System.Xml.Linq
open Fake.IO
open Fake.Core
open Snowflaqe.LinqToXmlExtensions

let path xs = Path.Combine(Array.ofList xs)

let solutionRoot = Files.findParent __SOURCE_DIRECTORY__ "Snowflaqe.sln";

let src = path [ solutionRoot; "src" ]
let tests = path [ solutionRoot; "tests" ]
let tasks = path [ solutionRoot; "tasks" ]

let [<Literal>] TargetFramework = "net6.0"

let test() =
    if Shell.Exec(Tools.dotnet, "run", tests) <> 0
    then failwith "tests failed"

let build() =
    if Shell.Exec(Tools.dotnet, "build --configuration Release", solutionRoot) <> 0
    then failwith "tests failed"

/// <summary>
/// Packs Snowflaqe CLI and installs it locally for testing
/// </summary>
let pack() =
    Shell.deleteDir (path [ "src"; "bin" ])
    Shell.deleteDir (path [ "src"; "obj" ])
    if Shell.Exec(Tools.dotnet, "msbuild /t:pack Snowflaqe.fsproj -p:IsNuget=true -p:Configuration=Release", src) <> 0 then
        failwith "Pack failed"
    else
        let outputPath = path [ src; "bin"; "Release" ]
        try
            // try get the version to see if Snowflaqe is already installed
            Shell.Exec("snowflaqe", "--version") |> ignore
            printfn "Snowflaqe is already installed globally, uninstalling..."
            if Shell.Exec(Tools.dotnet, "tool uninstall snowflaqe -g") = 0 then
                if Shell.Exec(Tools.dotnet, sprintf "tool install -g snowflaqe --add-source %s" outputPath) <> 0
                then failwith "Local install failed"
            else
                failwith "Failed to uninstall existing Snowflaqe"
        with
        | _ ->
            // install snowflaqe
            if Shell.Exec(Tools.dotnet, sprintf "tool install -g snowflaqe --add-source %s" outputPath) <> 0
            then failwith "Local install failed"

let packMSBuildTask () =
    Shell.deleteDir (path [ "tasks"; "bin" ])
    Shell.deleteDir (path [ "tasks"; "obj" ])
    if Shell.Exec(Tools.dotnet, "msbuild /t:restore -p:Configuration=Release", tasks) = 0 then
        if Shell.Exec(Tools.dotnet, "msbuild /t:pack -p:Configuration=Release", tasks) <> 0 then
            failwith "Packing Snowflaqe.Tasks failed"
    else
        failwith "Restoring the tasks project failed"

let publish() =
    Shell.deleteDir (path [ src; "bin" ])
    Shell.deleteDir (path [ src; "obj" ])

    if Shell.Exec(Tools.dotnet, "msbuild /t:restore Snowflaqe.fsproj -p:IsNuget=true -p:Configuration=Release", src) = 0 then

        if Shell.Exec(Tools.dotnet, "msbuild /t:pack Snowflaqe.fsproj -p:IsNuget=true -p:Configuration=Release", src) <> 0 then
            failwith "Pack failed"
        else
            let nugetKey =
                match Environment.environVarOrNone "NUGET_KEY" with
                | Some nugetKey -> nugetKey
                | None -> failwith "The Nuget API key must be set in a NUGET_KEY environmental variable"

            let nugetPath =
                Directory.GetFiles(path [ src; "bin"; "Release" ])
                |> Seq.head
                |> Path.GetFullPath

            if Shell.Exec(Tools.dotnet, sprintf "nuget push %s -s https://api.nuget.org/v3/index.json -k %s" nugetPath nugetKey, src) <> 0
            then failwith "Publish failed"
    else
        failwith "Failed to restore Snowflaqe.fsproj"

let publishTasks() =
    Shell.deleteDir (path [ tasks; "bin" ])
    Shell.deleteDir (path [ tasks; "obj" ])

    if Shell.Exec(Tools.dotnet, "msbuild /t:restore -p:Configuration=Release", tasks) = 0 then
        if Shell.Exec(Tools.dotnet, "msbuild /t:pack Snowflaqe.Tasks.fsproj -p:Configuration=Release", tasks) <> 0 then
            failwith "Packing Snowflaqe.Tasks failed"
        else
            let nugetKey =
                match Environment.environVarOrNone "NUGET_KEY" with
                | Some nugetKey -> nugetKey
                | None -> failwith "The Nuget API key must be set in a NUGET_KEY environmental variable"

            let nugetPath =
                Directory.GetFiles(path [ tasks; "bin"; "Release" ])
                |> Seq.head
                |> Path.GetFullPath

            if Shell.Exec(Tools.dotnet, sprintf "nuget push %s -s https://api.nuget.org/v3/index.json -k %s" nugetPath nugetKey, tasks) <> 0
            then failwith "Publishing Snowflaqe.Tasks failed"
    else
        failwith "Building Snowflaqe.Tasks failed"

let buildCraftSchema() =
    if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} --project Snowflaqe.fsproj -- --config ../samples/craft-cms/snowflaqe.json --generate", path [ solutionRoot; "src" ]) <> 0 then
        failwith "Running Fable generation failed"
    else
        if Shell.Exec(Tools.dotnet, "build", path [ solutionRoot; "samples"; "craft-cms"; "output" ]) <> 0
        then failwith "Could not build generated CraftCMS"

let buildGithub() =
    if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} --project Snowflaqe.fsproj -- --config ../samples/github/snowflaqe.json --generate", path [ solutionRoot; "src" ]) <> 0
    then failwith "Failed to generate Github client"
    elif Shell.Exec(Tools.dotnet, "build Github.fsproj", path [ solutionRoot; "samples"; "github"; "output" ]) <> 0
    then failwith "Failed to build the generated Github project"

let buildGithubDotNet() =
    if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} --project Snowflaqe.fsproj -- --config ../samples/github/snowflaqe-dot-net.json --generate", path [ solutionRoot; "src" ]) <> 0
    then failwith "Failed to generate Github client"
    elif Shell.Exec(Tools.dotnet, "build Github.Data.GraphQLClient.fsproj", path [ solutionRoot; "samples"; "github"; "output" ]) <> 0
    then failwith "Failed to build the generated Github project"

let buildGithubFable() =
    if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} --project Snowflaqe.fsproj -- --config ../samples/github/snowflaqe-fable.json --generate", path [ solutionRoot; "src" ]) <> 0
    then failwith "Failed to generate Github client"
    elif Shell.Exec(Tools.dotnet, "build Github.Data.GraphQLClient.fsproj", path [ solutionRoot; "samples"; "github"; "output" ]) <> 0
    then failwith "Failed to build the generated Github project"

let generateTasksUsings (targets: MSBuildTarget seq) =
    targets
    |> Seq.map (fun t -> t.Tasks)
    |> Seq.fold (fun folder tasks -> folder |> Seq.append tasks) Seq.empty
    |> Seq.groupBy (fun task -> (task.FullName, task.AssemblyFile))
    |> Seq.map (fun (key, _) ->
        let (fullName, assemblyFile) = key
        MSBuildXElement.UsingTask(fullName, assemblyFile))

let createPackageReference name version =
    {  Name = name
       Version = version
       PrivateAssets = ValueNone
       IncludeAssets = ValueNone }

let generateProjectFile (imports: string seq) (defaultTargets: string option) (targets: MSBuildTarget seq)=
    XDocument(
        XElement.ofStringName("Project",
            XAttribute.ofStringName("Sdk", "Microsoft.NET.Sdk"),
            seq {
                if defaultTargets.IsSome
                then yield XAttribute.ofStringName("DefaultTargets", defaultTargets.Value) :> obj
                yield! imports |> Seq.map (fun path -> MSBuildXElement.Import(path) :> obj)
                yield XElement.ofStringName("PropertyGroup",
                        XElement.ofStringName("OutputType", "Exe"),
                        XElement.ofStringName("TargetFramework", "net6.0")) :> obj
                yield! targets |> Seq.map (fun target -> MSBuildXElement.Target(target) :> obj)
            }))

let generateProjectFileForTask (targetFrameworks : string list) (package :MSBuildPackageReference list) (project : string) (schema: string) (target : string) (queries : string) =

    let packageReferences = [
        {
            Name = "Snowflaqe.Tasks"
            Version = Program.SnowflaqeTasksVersion
            PrivateAssets = ValueSome "all"
            IncludeAssets = ValueSome "build"
        };
        createPackageReference "FSharp.Control.FusionTasks" "2.6.0";
        createPackageReference "FSharp.SystemTextJson" Program.FSharpSystemTextJsonVersion;
        createPackageReference "System.Net.Http.Json" Program.SystemNetHttpJsonVersion;
    ]

    let packageReferences = packageReferences @ package
    XDocument(
        XElement.ofStringName("Project",
            XAttribute.ofStringName("Sdk", "Microsoft.NET.Sdk"),
            seq {
                yield XElement.ofStringName("PropertyGroup",
                        XElement.ofStringName("OutputType", "Exe"),
                        match targetFrameworks with
                        | [] -> XElement.ofStringName("TargetFramework", "net6.0")
                        | [head] -> XElement.ofStringName("TargetFramework", head)
                        | frameworks -> XElement.ofStringName("TargetFrameworks", frameworks |> String.concat(";"))
                ) :> obj

                yield XElement.ofStringName("PropertyGroup", seq {
                    XElement.ofStringName("SnowflaqeProjectName", project)
                    XElement.ofStringName("SnowflaqeQueriesFolder", queries)
                    XElement.ofStringName("SnowflaqeSchemaPath", schema)
                    XElement.ofStringName("SnowflaqeTargetProjectType", target)
                }) :> obj

                yield XElement.ofStringName("ItemGroup",
                    packageReferences
                    |> Seq.map (fun packageReference -> MSBuildXElement.Target(packageReference) :> obj)) :> obj
            }))

let createProjectFile imports defaultTargets targets (path: string) =
    let project = generateProjectFile imports defaultTargets targets
    project.WriteTo(path)

let createProjectFileAndRun imports (projectFileName : string) =
    createProjectFile imports None Seq.empty (path [ src; projectFileName ])

    if Shell.Exec(Tools.dotnet, $"build {projectFileName}", src) <> 0 then
        failwith $"Running {projectFileName} generation failed"
    Shell.Exec(Tools.dotnet, "clean Snowflaqe.fsproj -v q", src) |> ignore

let createProjectFileForTaskAndRun (directory: string) (project : string) (package :MSBuildPackageReference list) (schema : string) (target : string) (queries : string) =
    let projectFileName = project + ".fsproj"
    let project = generateProjectFileForTask
                    [ "net6.0" ]
                    package
                    project
                    schema
                    target
                    queries
    project.WriteTo (path [ directory; projectFileName ])

    if Shell.Exec(Tools.dotnet, $"build {projectFileName}", src) <> 0 then
        failwith $"Running {projectFileName} for task generation failed"
    Shell.Exec(Tools.dotnet, "clean Snowflaqe.fsproj -v q", src) |> ignore


let createMultiFrameworkProjectFileForTaskAndRun (directory: string) (project : string)  (packages :MSBuildPackageReference list) (schema : string) (target : string) (queries : string) =
    let projectFileName = project + ".fsproj"
    let project = generateProjectFileForTask
                    [ "netcoreapp3.1"; "net6.0" ]
                    packages
                    project
                    schema
                    target
                    queries
    project.WriteTo (path [ directory; projectFileName ])

    if Shell.Exec(Tools.dotnet, $"build {projectFileName}", src) <> 0 then
        failwith $"Running {projectFileName} generation failed"
    Shell.Exec(Tools.dotnet, "clean Snowflaqe.fsproj -v q", src) |> ignore

let createProjectFileAndRebuild (directory: string) (project : string) (packages :MSBuildPackageReference list) (schema: string) (target: string) (queries: string) =
    let projectFileName = project + ".fsproj"
    let project = generateProjectFileForTask
                    [ "net6.0" ]
                    packages
                    project
                    schema
                    target
                    queries
    project.Save (path [ directory; projectFileName ])
    if Shell.Exec(Tools.dotnet, $"build {projectFileName} --no-incremental", src) <> 0 then
        failwith $"Running {projectFileName} generation failed"
    if Shell.Exec(Tools.dotnet, $"build {projectFileName}", src) <> 0 then
        failwith $"Running {projectFileName} generation failed"
    Shell.Exec(Tools.dotnet, "clean Snowflaqe.fsproj -v q", src) |> ignore

let tasksIntegration() =
    let generateGQLClientTask =
        { Name = "GenerateGraphQLClient"
          FullName = "Snowflaqe.Tasks.GenerateGraphQLClient"
          AssemblyFile = path [ solutionRoot; "tasks"; "bin"; "Release"; "net6.0"; "Snowflaqe.Tasks.dll" ]
          Parameters =
            seq {
                KeyValuePair("Output", path [ solutionRoot; "src"; "output"] :> obj)
                KeyValuePair("Project", "Spotify" :> obj)
                KeyValuePair("Queries", path [ solutionRoot; "src"; "queries"] :> obj)
                KeyValuePair("Schema", path [ solutionRoot; "src"; "spotify-schema.json"] :> obj)
            }}
    createProjectFile
        Seq.empty
        None
        ({  Name = "GenerateGraphQLClient"
            AfterTargets = ValueNone
            BeforeTargets = ValueSome "Rebuild"
            Tasks = (generateGQLClientTask |> Seq.singleton) } |> Seq.singleton)
        (path [ solutionRoot; "src"; "SpotifyWithTasks.fsproj"])

    if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} --project Snowflaqe.fsproj -- --config snowflaqe-props-task.json --generate", src) <> 0 then
        failwith "Running Fable props generation failed"

    createProjectFileAndRebuild
        src
        "Spotify"
        [
            createPackageReference "Fable.SimpleHttp" Program.FableSimpleHttpVersion
            createPackageReference "Fable.SimpleJson" Program.FableSimpleJsonVersion
        ]
        "spotify-schema.json"
        "fable"
        "queries"

    if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} --project Snowflaqe.fsproj -- --config ./snowflaqe-fsharp-props-task.json --generate", src) <> 0 then
        failwith "Running FSharp props generation failed"

    createProjectFileAndRebuild
        src
        "Spotify"
        []
        "spotify-schema.json"
        "fsharp"
        "queries"

    if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} --project Snowflaqe.fsproj -- --config ./snowflaqe-shared-props-task.json --generate", src) <> 0 then
        failwith "Running Shared props generation failed"

    createProjectFileAndRebuild src
        "Spotify.Fable"
        [
            createPackageReference "Fable.SimpleHttp" Program.FableSimpleHttpVersion
            createPackageReference "Fable.SimpleJson" Program.FableSimpleJsonVersion
        ]
        "spotify-schema.json"
        "shared"
        "queries"
    createProjectFileAndRebuild
        src
        "Spotify.DotNet"
        [
            createPackageReference "FSharp.SystemTextJson" Program.FSharpSystemTextJsonVersion
            createPackageReference "Fable.Remoting.Json" Program.FableRemotingJsonVersion
        ]
        "spotify-schema.json"
        "shared"
        "queries"


let multiFrameworkTasksIntegration() =
    let generateGQLClientTask =
        { Name = "GenerateGraphQLClient"
          FullName = "Snowflaqe.Tasks.GenerateGraphQLClient"
          AssemblyFile = path [ solutionRoot; "tasks"; "bin"; "Release"; "net6.0"; "Snowflaqe.Tasks.dll" ]
          Parameters =
            seq {
                KeyValuePair("Output", path [ solutionRoot; "src"; "output"] :> obj)
                KeyValuePair("Project", "Spotify" :> obj)
                KeyValuePair("Queries", path [ solutionRoot; "src"; "queries"] :> obj)
                KeyValuePair("Schema", path [ solutionRoot; "src"; "spotify-schema.json"] :> obj)
            }}
    createProjectFile
        Seq.empty
        None
        ({  Name = "GenerateGraphQLClient"
            AfterTargets = ValueNone
            BeforeTargets = ValueSome "Rebuild"
            Tasks = (generateGQLClientTask |> Seq.singleton) } |> Seq.singleton)
        (path [ solutionRoot; "src"; "SpotifyWithTasks.fsproj"])

    if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} --project Snowflaqe.fsproj -- --config snowflaqe-props-task.json --generate", src) <> 0 then
        failwith "Running Fable props generation failed"

    createMultiFrameworkProjectFileForTaskAndRun
        src
        "Spotify"
        [
            createPackageReference "Fable.SimpleHttp" Program.FableSimpleHttpVersion
            createPackageReference "Fable.SimpleJson" Program.FableSimpleJsonVersion
        ]
        "spotify-schema.json"
        "fable"
        "queries"

    if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} --project Snowflaqe.fsproj -- --config ./snowflaqe-fsharp-props-task.json --generate", src) <> 0 then
        failwith "Running FSharp props generation failed"

    createMultiFrameworkProjectFileForTaskAndRun
        src
        "Spotify"
        [
            createPackageReference "FSharp.SystemTextJson" Program.FSharpSystemTextJsonVersion
            createPackageReference "Fable.Remoting.Json" Program.FableRemotingJsonVersion
        ]
        "spotify-schema.json"
        "fsharp"
        "queries"

    if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} --project Snowflaqe.fsproj -- --config ./snowflaqe-shared-props-task.json --generate", src) <> 0 then
        failwith "Running Shared props generation failed"

    createMultiFrameworkProjectFileForTaskAndRun
        src
        "Spotify.Fable"
        [
            createPackageReference "Fable.SimpleHttp" Program.FableSimpleHttpVersion
            createPackageReference "Fable.SimpleJson" Program.FableSimpleJsonVersion
        ]
        "spotify-schema.json"
        "shared"
        "queries"
    createMultiFrameworkProjectFileForTaskAndRun
        src
        "Spotify.DotNet"
        [
            createPackageReference "FSharp.SystemTextJson" Program.FSharpSystemTextJsonVersion
            createPackageReference "Fable.Remoting.Json" Program.FableRemotingJsonVersion
        ]
        "spotify-schema.json"
        "shared"
        "queries"

let generate config =
    Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} --project Snowflaqe.fsproj -- --config ./{config} --generate", src)

let propsIntegration() =
    Console.WriteLine "Generating Fable props"
    if generate "snowflaqe-props.json" <> 0 then
        failwith "Running Fable props generation failed"

    createProjectFileAndRun ("./output/Spotify.props" |> Seq.singleton) "Spotify.fsproj"

    Console.WriteLine "Generating F# props"
    if generate "snowflaqe-fsharp-props.json" <> 0 then
        failwith "Running FSharp props generation failed"

    createProjectFileAndRun ("./output/Spotify.props" |> Seq.singleton) "Spotify.fsproj"

    if generate "snowflaqe-shared-props.json" <> 0 then
        failwith "Running Shared props generation failed"

    Console.WriteLine "Generating Shared props"
    createProjectFileAndRun
        (seq {
            "./output/shared/Spotify.props"
            "./output/fable/Spotify.props"
        })
        "Spotify.Fable.fsproj"

    createProjectFileAndRun
        (seq {
            "./output/shared/Spotify.props"
            "./output/dotnet/Spotify.props"
        })
        "Spotify.DotNet.fsproj"

let buildFSharpWithTasks() =
    if generate "./snowflaqe-fsharp-task.json" <> 0 then
        failwith "Running FSharp generation with tasks failed"
    else
        if Shell.Exec(Tools.dotnet, "build", path [ solutionRoot; "src"; "output" ]) <> 0
        then failwith "Building generated F# project with tasks project failed"

let buildFSharpWithSystemTextJson() =
    if generate "./snowflaqe-fsharp-system-serializer.json" <> 0 then
        failwith "Running FSharp generation with tasks with system text json failed"
    else
        if Shell.Exec(Tools.dotnet, "build", path [ solutionRoot; "src"; "output" ]) <> 0
        then failwith "Building generated F# project tasks with system text json project failed"

let buildFSharpShared() =
    if generate "snowflaqe-shared.json" <> 0 then
        failwith "Running Shared project generation failed"
    else
        let output = List.sum [
            Shell.Exec(Tools.dotnet, "build", path [ solutionRoot; "src"; "output"; "shared" ])
            Shell.Exec(Tools.dotnet, "build", path [ solutionRoot; "src"; "output"; "fable" ])
            Shell.Exec(Tools.dotnet, "build", path [ solutionRoot; "src"; "output"; "dotnet" ])
        ]

        if output <> 0 then failwith "Building generated shared projects failed"

let buildFSharpWithSharedTasks() =
    if generate "snowflaqe-shared-task.json" <> 0 then
        failwith "Running FSharp generation with tasks failed"
    else
        let output = List.sum [
            Shell.Exec(Tools.dotnet, "build", path [ solutionRoot; "src"; "output"; "shared" ])
            Shell.Exec(Tools.dotnet, "build", path [ solutionRoot; "src"; "output"; "fable" ])
            Shell.Exec(Tools.dotnet, "build", path [ solutionRoot; "src"; "output"; "dotnet" ])
        ]

        if output <> 0 then failwith "Building generated shared projects failed"

let fsprojIntegration() =
    Console.WriteLine "Generating Fable project"
    if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} --project Snowflaqe.fsproj -- --generate", path [ solutionRoot; "src" ]) <> 0 then
        failwith "Running Fable generation failed"

    if Shell.Exec(Tools.dotnet, "build", path [ solutionRoot; "src"; "output" ]) <> 0
    then failwith "Building generated Fable project failed"

    Console.WriteLine "Generating F# project"
    if generate "snowflaqe-fsharp.json" <> 0 then
        failwith "Running FSharp generation failed"

    if Shell.Exec(Tools.dotnet, "build", path [ solutionRoot; "src"; "output" ]) <> 0
    then failwith "Building generated FSharp project failed"

    Console.WriteLine "Generating Shared project"
    if generate "snowflaqe-shared.json" <> 0 then
        failwith "Running Shared project generation failed"

    let output = List.sum [
        Shell.Exec(Tools.dotnet, "build", path [ solutionRoot; "src"; "output"; "shared" ])
        Shell.Exec(Tools.dotnet, "build", path [ solutionRoot; "src"; "output"; "fable" ])
        Shell.Exec(Tools.dotnet, "build", path [ solutionRoot; "src"; "output"; "dotnet" ])
    ]

    if output <> 0 then failwith "Building generated shared projects failed"
    else
        buildFSharpWithTasks()
        buildFSharpShared()
        buildFSharpWithSharedTasks()
        buildFSharpWithSystemTextJson()
        buildGithub()
        buildCraftSchema()
        buildGithubDotNet()
        buildGithubFable()

let clear() =
    File.Delete(path [ solutionRoot; "src"; "nuget.config" ])
    File.Delete(path [ solutionRoot; "src"; "Spotify.fsproj" ])
    File.Delete(path [ solutionRoot; "src"; "Spotify.DotNet.fsproj" ])
    File.Delete(path [ solutionRoot; "src"; "Spotify.Fable.fsproj" ])
    File.Delete(path [ solutionRoot; "src"; "SpotifyWithTasks.fsproj" ])
    Shell.deleteDir (path [ solutionRoot; "src"; "output" ])
    printfn "Cleared build artifacts"

let integration() =
    packMSBuildTask()
    tasksIntegration()
    clear()
    multiFrameworkTasksIntegration()
    clear()
    propsIntegration()
    clear()
    fsprojIntegration()
    clear()

[<EntryPoint>]
let main (args: string[]) =
    Console.InputEncoding <- Encoding.UTF8
    Console.OutputEncoding <- Encoding.UTF8
    Console.WriteLine(Swag.logo)
    try
        match args with
        | [| "build"   |] -> build()
        | [| "test"    |] -> test()
        | [| "pack"    |] -> pack()
        | [| "pack-tasks" |] -> packMSBuildTask()
        | [| "publish" |] -> publish()
        | [| "publish-tasks" |] -> publishTasks()
        | [| "integration" |] -> integration()
        | [| "fsproj-integration" |] -> 
            clear()
            fsprojIntegration()
            clear()
        | [| "build-craft" |] -> buildCraftSchema()
        | [| "build-github" |] -> buildGithub()
        | [| "build-github-dot-project" |] -> buildGithubDotNet()
        | [| "build-github-fable" |] -> buildGithubFable()
        | [| "clear" |] -> clear()
        | _ -> printfn "Unknown args %A" args
        0
    with ex ->
        printfn "%A" ex
        1
