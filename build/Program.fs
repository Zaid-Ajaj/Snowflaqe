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

let [<Literal>] TargetFramework = "netcoreapp3.1"

let test() =
    if Shell.Exec(Tools.dotnet, "run", tests) <> 0
    then failwith "tests failed"

let build() =
    if Shell.Exec(Tools.dotnet, "build --configuration Release", solutionRoot) <> 0
    then failwith "tests failed"

let pack() =
    Shell.deleteDir (path [ "src"; "bin" ])
    Shell.deleteDir (path [ "src"; "obj" ])
    if Shell.Exec(Tools.dotnet, "pack --configuration Release", src) <> 0 then
        failwith "Pack failed"
    else
        let outputPath = path [ src; "bin"; "Release" ]
        if Shell.Exec(Tools.dotnet, sprintf "tool install -g snowflaqe --add-source %s" outputPath) <> 0
        then failwith "Local install failed"

let packTask () =
    Shell.deleteDir (path [ "tasks"; "bin" ])
    Shell.deleteDir (path [ "tasks"; "obj" ])
    if Shell.Exec(Tools.dotnet, "pack --configuration Release", tasks) <> 0 then
        failwith "Pack failed"

let publish() =
    Shell.deleteDir (path [ src; "bin" ])
    Shell.deleteDir (path [ src; "obj" ])

    if Shell.Exec(Tools.dotnet, "pack --configuration Release", src) <> 0 then
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

        if Shell.Exec(Tools.dotnet, sprintf "nuget push %s -s nuget.org -k %s" nugetPath nugetKey, src) <> 0
        then failwith "Publish failed"

let buildCraftSchema() =
    if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} -p Snowflaqe.fsproj -- --config ../samples/craft-cms/snowflaqe.json --generate", path [ solutionRoot; "src" ]) <> 0 then
        failwith "Running Fable generation failed"
    else
        if Shell.Exec(Tools.dotnet, "build", path [ solutionRoot; "samples"; "craft-cms"; "output" ]) <> 0
        then failwith "Could not build generated CraftCMS"

let buildGithub() =
    if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} -p Snowflaqe.fsproj -- --config ../samples/github/snowflaqe.json --generate", path [ solutionRoot; "src" ]) <> 0
    then failwith "Failed to generate Github client"
    elif Shell.Exec(Tools.dotnet, "build Github.fsproj", path [ solutionRoot; "samples"; "github"; "output" ]) <> 0
    then failwith "Failed to build the generated Github project"

let buildGithubDotProject() =
    if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} -p Snowflaqe.fsproj -- --config ../samples/github/snowflaqe-dot-project.json --generate", path [ solutionRoot; "src" ]) <> 0
    then failwith "Failed to generate Github client"
    elif Shell.Exec(Tools.dotnet, "build Github.Data.GraphQLClient.fsproj", path [ solutionRoot; "samples"; "github"; "output" ]) <> 0
    then failwith "Failed to build the generated Github project"

let buildGithubFable() =
    if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} -p Snowflaqe.fsproj -- --config ../samples/github/snowflaqe-fable.json --generate", path [ solutionRoot; "src" ]) <> 0
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
                        XElement.ofStringName("TargetFramework", "netcoreapp3.1")) :> obj
                yield! generateTasksUsings targets |> Seq.map (fun u -> u :> obj)
                yield! targets
                |> Seq.map
                    (fun target -> MSBuildXElement.Target(target) :> obj)
            }))

let generateProjectFileForTask ()=
    let createPackageReference name version =
        {  Name = name
           Version = version
           PrivateAssets = None
           IncludeAssets = None}
    let packageReferences =  [{  Name = "Snowflaqe.Tasks"
                                 Version = "1.0.0"
                                 PrivateAssets = Some "all"
                                 IncludeAssets = Some "build"};
                                 createPackageReference "FSharp.Control.FusionTasks" "2.4.0";
                                 createPackageReference "FSharp.SystemTextJson" Program.FSharpSystemTextJsonVersion;
                                 createPackageReference "System.Net.Http.Json" Program.SystemNetHttpJsonVersion;]
    XDocument(
        XElement.ofStringName("Project",
            XAttribute.ofStringName("Sdk", "Microsoft.NET.Sdk"),
            seq {
                yield XElement.ofStringName("PropertyGroup",
                        XElement.ofStringName("OutputType", "Exe"),
                        XElement.ofStringName("TargetFramework", "netcoreapp3.1")) :> obj
                yield XElement.ofStringName("ItemGroup",
                    packageReferences
                    |> Seq.map (fun packageReference -> MSBuildXElement.Target(packageReference) :> obj)) :> obj
            }))

let createProjectFile imports defaultTargets targets (path: string) =
    let project = generateProjectFile imports defaultTargets targets
    project.WriteTo(path)

let createProjectFileAndRun imports defaultTargets (directory: string) (projectFileName : string) =
    createProjectFile imports defaultTargets Seq.empty (path[directory; projectFileName])
    let p = path [ solutionRoot; "src"; "output"; "nuget.config"]
    let p1 = path[directory; projectFileName]

    if Shell.Exec(Tools.dotnet, $"build {projectFileName}", src) <> 0 then
        failwith $"Running {projectFileName} generation failed"
    //if Shell.Exec(Tools.dotnet,$"dotnet restore {p1} -s { p }", src) <> 0 then
    //    failwith "Running Fable props generation failed"
    Shell.Exec(Tools.dotnet, "clean Snowflaqe.fsproj -v q", src) |> ignore

let createProjectFileForTaskAndRun (directory: string) (projectFileName : string) =
    let project = generateProjectFileForTask ()
    project.WriteTo(path[directory; projectFileName])

    if Shell.Exec(Tools.dotnet, $"build {projectFileName}", src) <> 0 then
        failwith $"Running {projectFileName} generation failed"
    Shell.Exec(Tools.dotnet, "clean Snowflaqe.fsproj -v q", src) |> ignore

let createProjectFileAndRebuild (directory: string) (projectFileName : string) =
    let project = generateProjectFileForTask ()
    project.Save (path[directory; projectFileName])
    if Shell.Exec(Tools.dotnet, $"build {projectFileName} --no-incremental", src) <> 0 then
        failwith $"Running {projectFileName} generation failed"
    if Shell.Exec(Tools.dotnet, $"build {projectFileName}", src) <> 0 then
        failwith $"Running {projectFileName} generation failed"
    Shell.Exec(Tools.dotnet, "clean Snowflaqe.fsproj -v q", src) |> ignore

let tasksIntegration() =
    let generateGQLClientTask =
        { Name = "GenerateGraphQLClient"
          FullName = "Snowflaqe.Tasks.GenerateGraphQLClient"
          AssemblyFile = path [ solutionRoot; "tasks"; "bin"; "Release"; "netcoreapp3.1"; "Snowflaqe.Tasks.dll" ]
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
            AfterTargets = None
            BeforeTargets = Some "Rebuild"
            Tasks = (generateGQLClientTask |> Seq.singleton) } |> Seq.singleton)
        (path [ solutionRoot; "src"; "SpotifyWithTasks.fsproj"])

    if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} -p Snowflaqe.fsproj -- --config snowflaqe-props-task.json --generate", src) <> 0 then
        failwith "Running Fable props generation failed"
    else
        createProjectFileAndRebuild
            src
            "Spotify.fsproj"

        if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} -p Snowflaqe.fsproj -- --config ./snowflaqe-fsharp-props-task.json --generate", src) <> 0 then
            failwith "Running FSharp props generation failed"
        else
        createProjectFileAndRebuild
            src
            "Spotify.fsproj"

        if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} -p Snowflaqe.fsproj -- --config ./snowflaqe-shared-props-task.json --generate", src) <> 0 then
            failwith "Running Shared props generation failed"
        else
            createProjectFileAndRebuild
                src
                "Spotify.Fable.fsproj"

            createProjectFileAndRebuild
                src
                "Spotify.Dotnet.fsproj"

let generate config =
    Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} -p Snowflaqe.fsproj -- --config ./{config} --generate", src)

let propsIntegration() =
    Console.WriteLine "Generating Fable props"
    if generate "snowflaqe-props.json" <> 0 then
        failwith "Running Fable props generation failed"
    else
        createProjectFileAndRun
            ("./output/Spotify.props" |> Seq.singleton)
            None
            src
            "Spotify.fsproj"

        Console.WriteLine "Generating F# props"
        if generate "snowflaqe-fsharp-props.json" <> 0 then
            failwith "Running FSharp props generation failed"
        else
        createProjectFileAndRun
            ("./output/Spotify.props" |> Seq.singleton)
            None
            src
            "Spotify.fsproj"

        if generate "snowflaqe-shared-props.json" <> 0 then
            failwith "Running Shared props generation failed"
        else
            Console.WriteLine "Generating Shared props"
            createProjectFileAndRun
                (seq {
                    "./output/shared/Spotify.props"
                    "./output/fable/Spotify.props"
                })
                None
                src
                "Spotify.Fable.fsproj"

            createProjectFileAndRun
                (seq {
                    "./output/shared/Spotify.props"
                    "./output/dotnet/Spotify.props"
                })
                None
                src
                "Spotify.Dotnet.fsproj"

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
    if Shell.Exec(Tools.dotnet, $"run -f {TargetFramework} -p Snowflaqe.fsproj -- --generate", path [ solutionRoot; "src" ]) <> 0 then
        failwith "Running Fable generation failed"
    else
        if Shell.Exec(Tools.dotnet, "build", path [ solutionRoot; "src"; "output" ]) <> 0
        then failwith "Building generated Fable project failed"
        else
            Console.WriteLine "Generating F# project"
            if generate "snowflaqe-fsharp.json" <> 0 then
                failwith "Running FSharp generation failed"
            else
            if Shell.Exec(Tools.dotnet, "build", path [ solutionRoot; "src"; "output" ]) <> 0
            then failwith "Building generated FSharp project failed"
            else
                Console.WriteLine "Generating Shared project"
                if generate "snowflaqe-shared.json" <> 0 then
                    failwith "Running Shared project generation failed"
                else
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
                        buildGithubDotProject()
                        buildGithubFable()

let clear() =
    File.Delete(path [ solutionRoot; "src"; "nuget.config" ])
    File.Delete(path [ solutionRoot; "src"; "Spotify.fsproj" ])
    File.Delete(path [ solutionRoot; "src"; "Spotify.Dotnet.fsproj" ])
    File.Delete(path [ solutionRoot; "src"; "Spotify.Fable.fsproj" ])
    File.Delete(path [ solutionRoot; "src"; "SpotifyWithTasks.fsproj" ])
    Directory.Delete(path [ solutionRoot; "src"; "output" ], true)

let integration() =
    packTask()
    tasksIntegration()
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
        | [| "publish" |] -> publish()
        | [| "integration" |] -> integration()
        | [| "build-craft" |] -> buildCraftSchema()
        | [| "build-github" |] -> buildGithub()
        | [| "build-github-dot-project" |] -> buildGithubDotProject()
        | [| "build-github-fable" |] -> buildGithubFable()

        | _ -> printfn "Unknown args %A" args
        0
    with ex ->
        printfn "%A" ex
        1
